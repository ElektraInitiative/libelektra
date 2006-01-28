// elektra.cpp : Defines the entry point for the DLL application.
//
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "kdb.h"

/* Entry point for when the dll is loaded and unloaded */
BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}