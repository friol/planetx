
//
// going mini
// friol 2o25
// - for size optimization: study NO_UNIFORM
// - part 2
// - experiment with more typefaces
//

#define WINDOWS_IGNORE_PACKING_MISMATCH

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
#include "shaders/fragment2.inl"
#include "shaders/post.inl"

#pragma data_seg(".pids")
// static allocation saves a few bytes
static int pidMain;
static int pidPost;
static int pidPart2;

void drawText(float posx, float posy, char* txt)
{
	((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidPost);
	((PFNGLUNIFORM1IPROC)wglGetProcAddress("glUniform1i"))(0, 0);
	glRasterPos2f(posx,posy);
	glCallLists(strlen(txt), GL_UNSIGNED_BYTE, txt);
	glFinish();
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
	// initialize window
	#if FULLSCREEN
		ChangeDisplaySettings(&screenSettings, CDS_FULLSCREEN);
		ShowCursor(0);
		const HDC hDC = GetDC(CreateWindow((LPCSTR)0xC018, 0, WS_POPUP | WS_VISIBLE | WS_MAXIMIZE, 0, 0, 0, 0, 0, 0, 0, 0));
	#else
		#ifdef EDITOR_CONTROLS
			HWND window = CreateWindow("static", 0, WS_POPUP | WS_VISIBLE, 0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN),
				0, 0, 0, 0);
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
	((PFNGLSHADERSOURCEPROC)wglGetProcAddress("glShaderSource"))(debugid, 1, &part2, 0);
	((PFNGLCOMPILESHADERPROC)wglGetProcAddress("glCompileShader"))(debugid);
	((PFNGLGETSHADERIVPROC)wglGetProcAddress("glGetShaderiv"))(debugid, GL_COMPILE_STATUS, &result);
	if (result == GL_FALSE)
	{
		static char errorBuffer[4096];
		((PFNGLGETSHADERINFOLOGPROC)wglGetProcAddress("glGetShaderInfoLog"))(debugid, 4096 - 1, NULL, static_cast<char*>(errorBuffer));
		MessageBox(NULL, errorBuffer, "", 0x00000000L);
	}
#endif 

	pidMain = ((PFNGLCREATESHADERPROGRAMVPROC)wglGetProcAddress("glCreateShaderProgramv"))(GL_FRAGMENT_SHADER, 1, &fragment);
	pidPart2= ((PFNGLCREATESHADERPROGRAMVPROC)wglGetProcAddress("glCreateShaderProgramv"))(GL_FRAGMENT_SHADER, 1, &part2);
	pidPost = ((PFNGLCREATESHADERPROGRAMVPROC)wglGetProcAddress("glCreateShaderProgramv"))(GL_FRAGMENT_SHADER, 1, &post);

	// init font
	const HFONT mainFont = CreateFont(45, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ANTIALIASED_QUALITY, 0, "Tahoma");
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

		double position = 0.0;
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

			position = timeGetTime() - startTime;

			int curShadah = pidMain;
			//if (position < 30000) curShadah = pidMain;
			//((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(curShadah);

#if USE_AUDIO
			waveOutGetPosition(hWaveOut, &MMTime, sizeof(MMTIME));
#endif
			// it is possible to upload your vars as vertex color attribute (gl_Color) to save one function import
#if NO_UNIFORMS
			glColor3ui(MMTime.u.sample, 0, 0);
#endif

			int p0 = position;
			int p1 = GetSystemMetrics(SM_CXSCREEN);
			int p2 = GetSystemMetrics(SM_CYSCREEN);

#define FADESTART 28000
#define FADEEND 32000

			if (p0 < FADESTART)
			{
				((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidMain);
				glTexCoord3i(p0, p1, p2);
				glRects(-1, -1, 1, 1);
			}
			else if ((p0 >= FADESTART) && (p0 < FADEEND))
			{
				((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidPart2);
				glTexCoord3i(p0, p1, p2);
				float k = ((((float)p0 - FADESTART) / (FADESTART-FADEEND))*4.0);
				glBegin(GL_TRIANGLES);
				glVertex2f(0.0,k);
				glVertex2f(k*0.85, -k*1.3);
				glVertex2f(-k*0.85, -k*1.3);
				glEnd();
			}
			else
			{
				((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidPart2);
				glTexCoord3i(p0, p1, p2);
				glRects(-1, -1, 1, 1);
			}

			typedef struct
			{
				float px, py;
				int positionStart;
				int positionEnd;
				char* str;
			} textTimeline;

#define NUM_TEXTS 3
			textTimeline tlarr[NUM_TEXTS] =
			{
				{0.7f,-0.75f,6000,9000,"F  R  I  O  L"},
				{-0.95f,0.75f,12000,15000,"@REVISION 2o25"},
				{-0.2f,0,18000,34000,"P  L  A  N  E  T     X"},
			};

			for (unsigned int i = 0;i < NUM_TEXTS;i++)
			{
				if ((position >= tlarr[i].positionStart) && (position < tlarr[i].positionEnd))
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
				editor.updateShaders(&pidMain, &pidPost);
			#endif
	} 
	while(!GetAsyncKeyState(VK_ESCAPE)
		#if USE_AUDIO
			&& MMTime.u.sample < MAX_SAMPLES
		#endif
	);

	ExitProcess(0);
}
