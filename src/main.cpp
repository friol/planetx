
//
// going mini
// friol 2o25
// * for size optimization: study NO_UNIFORM
// * part 2
// * experiment with more typefaces
//

#define WINDOWS_IGNORE_PACKING_MISMATCH

#pragma warning( disable : 6031 6387)
#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#define VC_LEANMEAN
#define VC_EXTRALEAN

// custom build and feature flags
#ifdef DEBUG
	#define OPENGL_DEBUG        1
	#define FULLSCREEN          0
	#define DESPERATE           0
	#define BREAK_COMPATIBILITY 0
#else
	#define OPENGL_DEBUG        0
	#define FULLSCREEN          0
	#define DESPERATE           0
	#define BREAK_COMPATIBILITY 0
#endif

#define USE_MIPMAPS  1
#define USE_AUDIO    1
#define NO_UNIFORMS  0

#include "definitions.h"
#if OPENGL_DEBUG
	#include "debug.h"
#endif

#include "glext.h"
#include "shaders/fragment.inl"
//#include "shaders/fragment2.inl"

#pragma data_seg(".pids")
// static allocation saves a few bytes
static int pidMain;

void drawText(float posx, float posy, char* txt)
{
	//((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidPost);
	//((PFNGLUNIFORM1IPROC)wglGetProcAddress("glUniform1i"))(0, 0);
	((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(0);
	//glColor3f(1.0, 1.0, 1.0);
	
	glRasterPos2f(posx,posy);
	glCallLists(strlen(txt), GL_UNSIGNED_BYTE, txt);
}

void glHex(float x, float y,float k)
{
	float ln = k; float lnp04 = ln * 0.4; float lnp14 = ln * 1.4;
	glBegin(GL_POLYGON);
	glVertex2f(x+ln,y);
	glVertex2f(x+ lnp04,y+ lnp14);
	glVertex2f(x- lnp04,y+ lnp14);
	glVertex2f(x-ln,y);
	glVertex2f(x- lnp04,y- lnp14);
	glVertex2f(x+ lnp04,y- lnp14);
	glEnd();
}

#ifndef EDITOR_CONTROLS
#pragma code_seg(".main")
void entrypoint(void)
#else
#include "editor.h"
#include "song.h"
int __cdecl main(int argc, char* argv[])
#endif
{
	int p1 = GetSystemMetrics(SM_CXSCREEN);
	int p2 = GetSystemMetrics(SM_CYSCREEN);

	// initialize window
	#if FULLSCREEN
		ChangeDisplaySettings(&screenSettings, CDS_FULLSCREEN);
		ShowCursor(0);
		const HDC hDC = GetDC(CreateWindow((LPCSTR)0xC018, 0, WS_POPUP | WS_VISIBLE | WS_MAXIMIZE, 0, 0, 0, 0, 0, 0, 0, 0));
	#else
		#ifdef EDITOR_CONTROLS
			HWND window = CreateWindow("static", 0, WS_POPUP | WS_VISIBLE, 0, 0, p1, p2, 0, 0, 0, 0);
			HDC hDC = GetDC(window);
		#else
			// you can create a pseudo fullscreen window by similarly enabling the WS_MAXIMIZE flag as above
			// in which case you can replace the resolution parameters with 0s and save a couple bytes
			// this only works if the resolution is set to the display device's native resolution
			HDC hDC = GetDC(CreateWindow((LPCSTR)0xC018, 0, WS_POPUP | WS_VISIBLE, 0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN), 0, 0, 0, 0));
		#endif
	#endif

	// initalize opengl context
	SetPixelFormat(hDC, ChoosePixelFormat(hDC, &pfd), &pfd);
	wglMakeCurrent(hDC, wglCreateContext(hDC));
	
	// create and compile shader programs
#ifdef EDITOR_CONTROLS
	int result = 0;
	const int debugid = ((PFNGLCREATESHADERPROC)wglGetProcAddress("glCreateShader"))(GL_FRAGMENT_SHADER);
	((PFNGLSHADERSOURCEPROC)wglGetProcAddress("glShaderSource"))(debugid, 1, &fragment_frag, 0);
	((PFNGLCOMPILESHADERPROC)wglGetProcAddress("glCompileShader"))(debugid);
	((PFNGLGETSHADERIVPROC)wglGetProcAddress("glGetShaderiv"))(debugid, GL_COMPILE_STATUS, &result);
	if (result == GL_FALSE)
	{
		static char errorBuffer[4096];
		((PFNGLGETSHADERINFOLOGPROC)wglGetProcAddress("glGetShaderInfoLog"))(debugid, 4096 - 1, NULL, static_cast<char*>(errorBuffer));
		MessageBox(NULL, errorBuffer, "", 0x00000000L);
	}
#endif 

	pidMain = ((PFNGLCREATESHADERPROGRAMVPROC)wglGetProcAddress("glCreateShaderProgramv"))(GL_FRAGMENT_SHADER, 1, &fragment_frag);
	//pidPart2= ((PFNGLCREATESHADERPROGRAMVPROC)wglGetProcAddress("glCreateShaderProgramv"))(GL_FRAGMENT_SHADER, 1, &part2_frag);

	// init font
	const HFONT mainFont = CreateFont(45, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ANTIALIASED_QUALITY, 0, "Arial");
	//const HFONT mainFont = CreateFont(45, 0, 0, 0, FW_DONTCARE, FALSE, FALSE, FALSE, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_DONTCARE | DEFAULT_PITCH, "Verdana");
	SelectObject(hDC, mainFont);
	wglUseFontBitmaps(hDC, 0, 256, 0);

#ifdef EDITOR_CONTROLS
		Leviathan::Editor editor = Leviathan::Editor();
		//editor.updateShaders(&pidMain, &pidPost, true);
		//Leviathan::Song track(L"audio.wav");
#endif

#if USE_AUDIO
		CreateThread(0, 0, (LPTHREAD_START_ROUTINE)_4klang_render, lpSoundBuffer, 0, 0);
		waveOutOpen(&hWaveOut, WAVE_MAPPER, &WaveFMT, NULL, 0, CALLBACK_NULL);
		waveOutPrepareHeader(hWaveOut, &WaveHDR, sizeof(WaveHDR));
		waveOutWrite(hWaveOut, &WaveHDR, sizeof(WaveHDR));
#endif

		ShowCursor(0);

		unsigned long startTime = timeGetTime();

		// mainloop
		do
		{
#ifdef EDITOR_CONTROLS
			editor.beginFrame(timeGetTime());
#endif

#if !(DESPERATE)
			// do minimal message handling so windows doesn't kill your application
			// not always strictly necessary but increases compatibility and reliability a lot
			// normally you'd pass an msg struct as the first argument but it's just an
			// output parameter and the implementation presumably does a NULL check
			PeekMessage(0, 0, 0, 0, PM_REMOVE);
#endif

			int p0 = timeGetTime() - startTime;

#if USE_AUDIO
			waveOutGetPosition(hWaveOut, &MMTime, sizeof(MMTIME));
#endif
			// it is possible to upload your vars as vertex color attribute (gl_Color) to save one function import
#if NO_UNIFORMS
			glColor3ui(MMTime.u.sample, 0, 0);
#endif

#define FADESTART 28000
#define FADEEND 32000
#define FADEP3START 50000
#define FADEP3END 56000
#define REALFADEP3END 53000
#define PART4START 78000
#define HEXROWS 16
#define HEXCOLS 10
#define HEXSTART 10000
#define HEXEND 10000

			p0 += 27000;

			glLoadIdentity();
			((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidMain);
			//glTexCoord3i(p0, p1, p2);
			glTexCoord3i(p0, p1, p2);
			if ((p0 >= REALFADEP3END) && (p0 < PART4START))
			{
				glTexCoord3i(-p0, p1, p2);
			}
			glRects(-1, -1, 1, 1);

			if ((p0 >= FADESTART) && (p0 < FADEEND))
			{
				glTexCoord3i(p0+200000, p1, p2);
				float k = ((((float)p0 - FADESTART) / (FADESTART-FADEEND))*4.0f);
				glBegin(GL_TRIANGLES);
				glVertex2f(0.0f,k);
				glVertex2f(k*0.85f, -k*1.3f);
				glVertex2f(-k*0.85f, -k*1.3f);
				glEnd();
			}
			else if ((p0 >= FADEP3START) && (p0 < REALFADEP3END))
			{
				float k = (((((float)p0 - FADEP3START) / (FADEP3END - FADEP3START))))/8;

				glTexCoord3i(-p0, p1, p2);

				glScalef(3,3,3);

				float ypos = .4f;
				for (int r = 0;r < HEXROWS;r++)
				{
					float xpos = -0.8f;
					if (r%2) xpos -= 0.072f;
					for (int x = 0;x < HEXCOLS;x++)
					{
						glHex(xpos, ypos, k);
						xpos += 0.145f;
					}
					ypos -= 0.07f;
				}
			}

			typedef struct
			{
				float px, py;
				unsigned int positionStart;
				unsigned int positionEnd;
				char* str;
			} textTimeline;

#define NUM_TEXTS 20
			textTimeline tlarr[NUM_TEXTS] =
			{
				{0.8f,-0.75f,6000,9000,"FRIOL"},
				{-0.95f,0.75f,12000,15000,"REVISION2o25"},
				{-0.18f,0.02f,18000,30000,"P  L  A  N  E  T  X"},
				{0.5f,0.8f,54000,77000,"KEEP FLYING:"},
				{0.5f,0.7f,55000,77000,"SPINNING KIDS"},
				{0.5f,0.6f,56000,77000,"DEATHSTAR"},
				{0.5f,0.5f,57000,77000,"DARKAGE"},
				{0.5f,0.4f,58000,77000,"MERCURY"},
				{0.5f,0.3f,59000,77000,"CONSPIRACY"},
				{0.5f,0.2f,60000,77000,"FUTURE CREW"},
				{0.5f,0.1f,61000,77000,"ASD"},
				{0.5f,0.0f,62000,77000,"RGBA"},
				{0.5f,-0.1f,63000,77000,"TBL"},
				{0.5f,-0.2f,64000,77000,"JAPOTEK"},
				{0.5f,-0.3f,65000,77000,"RITUAL"},
				{0.5f,-0.4f,66000,77000,"ZERO DEFECTS"},
				{0.5f,-0.5f,67000,77000,"PAN"},
				{0.5f,-0.6f,68000,77000,"PELLICU$"},
				{0.5f,-0.7f,69000,77000,"FIZZER"},
				{0.5f,-0.8f,70000,77000,"AND YOU"},
			};

			for (unsigned int i = 0;i < NUM_TEXTS;i++)
			{
				if ((p0 >= tlarr[i].positionStart) && (p0 < tlarr[i].positionEnd))
				{
					drawText(tlarr[i].px,tlarr[i].py,tlarr[i].str);
				}
			}

			#if POST_PASS
				glBindTexture(GL_TEXTURE_2D, 1);
				#if USE_MIPMAPS
					glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
					glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 0, 0, XRES, YRES, 0);
					((PFNGLGENERATEMIPMAPPROC)wglGetProcAddress("glGenerateMipmap"))(GL_TEXTURE_2D);
				#else
					glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
					glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 0, 0, XRES, YRES, 0);
				#endif
				((PFNGLACTIVETEXTUREPROC)wglGetProcAddress("glActiveTexture"))(GL_TEXTURE0);
				((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidPost);
				((PFNGLUNIFORM1IPROC)wglGetProcAddress("glUniform1i"))(0, 0);
				glRects(-1, -1, 1, 1);
			#endif

			SwapBuffers(hDC);

			// handle functionality of the editor
			#ifdef EDITOR_CONTROLS
				editor.endFrame(timeGetTime());
				//position = editor.handleEvents(&track, position);
				editor.printFrameStatistics();
				//editor.updateShaders(&pidMain, &pidPost);
			#endif
	} 
	while(!GetAsyncKeyState(VK_ESCAPE)
		#if USE_AUDIO
			&& MMTime.u.sample < MAX_SAMPLES
		#endif
	);

	ExitProcess(0);
}
