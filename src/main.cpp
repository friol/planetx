
//
// going mini
// friol 2o25
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

#pragma data_seg(".pids")
// static allocation saves a few bytes
static int pidMain;

#ifndef EDITOR_CONTROLS
#pragma code_seg(".main")
void entrypoint(void)
#else
#include "editor.h"
#include "song.h"
int __cdecl main(int argc, char* argv[])
#endif
{
	unsigned int p1 = GetSystemMetrics(SM_CXSCREEN);
	unsigned int p2 = GetSystemMetrics(SM_CYSCREEN);

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
			HDC hDC = GetDC(CreateWindow((LPCSTR)0xC018, 0, WS_POPUP | WS_VISIBLE, 0, 0, p1,p2, 0, 0, 0, 0));
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

	// init font
	//45:1920=x:xres x=(xres*45)/1920
	const HFONT mainFont = CreateFont((p1 * 50) / 1920, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ANTIALIASED_QUALITY, 0, "Impact");
	SelectObject(hDC, mainFont);
	wglUseFontBitmaps(hDC, 0, 256, 0);

#ifdef EDITOR_CONTROLS
		Leviathan::Editor editor = Leviathan::Editor();
		//editor.updateShaders(&pidMain, &pidPost, true);
		//Leviathan::Song track(L"audio.wav");
#endif

		ShowCursor(0);

#if USE_AUDIO
		CreateThread(0, 0, (LPTHREAD_START_ROUTINE)_4klang_render, lpSoundBuffer, 0, 0);
		waveOutOpen(&hWaveOut, WAVE_MAPPER, &WaveFMT, NULL, 0, CALLBACK_NULL);
		waveOutPrepareHeader(hWaveOut, &WaveHDR, sizeof(WaveHDR));
		waveOutWrite(hWaveOut, &WaveHDR, sizeof(WaveHDR));
#endif

		unsigned long startTime = timeGetTime();
		unsigned int p0;

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

			p0 = timeGetTime() - startTime;

#if USE_AUDIO
			//waveOutGetPosition(hWaveOut, &MMTime, sizeof(MMTIME));
#endif
			// it is possible to upload your vars as vertex color attribute (gl_Color) to save one function import
#if NO_UNIFORMS
			glColor3ui(MMTime.u.sample, 0, 0);
#endif

			//p0 += 42000;

			((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(pidMain);
			((PFNGLUNIFORM1IPROC)wglGetProcAddress("glUniform1i"))(0,p0);
			((PFNGLUNIFORM1IPROC)wglGetProcAddress("glUniform1i"))(1,p1);
			((PFNGLUNIFORM1IPROC)wglGetProcAddress("glUniform1i"))(2,p2);

			glRects(-1, -1, 1, 1);

#define NUM_TEXTS 14
			char* tlarr[NUM_TEXTS] =
			{
				"F R I O L",
				"@REVISION 2o25",
				"P L A N E T  X",
				"KEEP FLYING:",
				"PELLICU$",
				"MOD3M",
				"FIZZER",
				"KB",
				"WRIGHTER",
				"RAZDEE",
				"MERCURY",
				"FUTURE CREW",
				"FLOPINE",
				"AND YOU.",
			};

			for (unsigned int i = 0;i < NUM_TEXTS;i++)
			{
				unsigned int pend = i == 0 ? 10000 : i == 1 ? 16000 : i == 2 ? 30000 : 77000;
				unsigned int pstart = i < 3 ? (i + 1) * 6000 : 54000 + ((i - 3) * 1500);
				float px = i == 0 ? 0.85f : i == 2 ? -0.075f : -0.95f;
				float py = i == 0 ? -0.85f : i == 1 ? 0.75f : i==2?0.0f: 0.2f+((2.0f - (float)i)) * 0.1f;
				if ((p0 >= pstart) && (p0 < pend))
				{
					((PFNGLUSEPROGRAMPROC)wglGetProcAddress("glUseProgram"))(0);
					glRasterPos2f(px,py);
					glCallLists(strlen(tlarr[i]), GL_UNSIGNED_BYTE, tlarr[i]);
				}
			}

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
			//&& MMTime.u.sample < MAX_SAMPLES
			&& p0<90000
		#endif
	);

	ExitProcess(0);
}
