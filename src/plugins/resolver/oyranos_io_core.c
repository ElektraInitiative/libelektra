/** @file oyranos_io_core.c
 *
 *  Oyranos is an open source Color Management System 
 *
 *  @par Copyright:
 *            2004-2014 (C) Kai-Uwe Behrmann
 *
 *  @brief    input / output  methods
 *  @internal
 *  @author   Kai-Uwe Behrmann <ku.b@gmx.de>
 *  @par License:
 *            new BSD <http://www.opensource.org/licenses/bsd-license.php>
 *  @since    2004/11/25
 */

#include <assert.h>
#include <errno.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "oyranos_config_internal.h"
#include "oyranos.h"
#include "oyranos_check.h"
#include "oyranos_debug.h"
#include "oyranos_helper.h"
#include "oyranos_internal.h"
#include "oyranos_io.h"
#include "oyranos_sentinel.h"
#include "oyranos_string.h"
#include "oyranos_xml.h"

/* --- Helpers  --- */

#define AD oyAllocateFunc_, oyDeAllocateFunc_

/* --- static variables   --- */

int oy_warn_ = 1;

/* --- structs, typedefs, enums --- */

/* --- internal API definition --- */


/* --- Helpers  --- */
/* small helpers */

/* --- function definitions --- */


size_t
oyReadFileSize_(const char* name)
{
  FILE *fp = 0;
  const char* filename = name;
  size_t size = 0;

  DBG_MEM_START

  {
    fp = fopen(filename, "rb");
    DBG_MEM2_S ("fp = %d filename = %s\n", (int)(intptr_t)fp, filename)

    if (fp)
    {
      /* get size */
      fseek(fp,0L,SEEK_END); 
      size = ftell (fp);
      fclose (fp);

    } else
      WARNc2_S( "%s: %s", _("Could not open profile"), filename );
  }

  DBG_MEM_ENDE
  return size;
}

/** @internal
 *  Read a file stream without knowing its size in advance.
 */
char *       oyReadFileSToMem_       ( FILE              * fp,
                                       size_t            * size,
                                       oyAlloc_f           allocate_func)
{
  size_t mem_size = 256;
  char* mem = malloc(mem_size),
        c;

  DBG_MEM_START

  DBG_MEM

  if (fp && size)
  {
    *size = 0;
    while((c = getc(fp)) && !feof(fp))
    {
      if(*size >= mem_size)
      {
        mem_size *= 2;
        mem = realloc( mem, mem_size );
      }
      mem[(*size)++] = c;
    }

    if(mem)
    {
      /* copy to external allocator */
      char* temp = mem;
      mem = oyAllocateWrapFunc_( *size+1, allocate_func );
      if(mem) {
        memcpy( mem, temp, *size );
        oyFree_m_ (temp)
        mem[*size] = 0;
      } else {
        oyFree_m_ (mem)
        *size = 0;
      }
    }
  }
 
  DBG_MEM_ENDE
  return mem;
}

char *       oyReadFilepToMem_       ( FILE              * fp,
                                       size_t            * size,
                                       oyAlloc_f           allocate_func)
{
  char* mem = NULL;

  DBG_MEM_START

  DBG_MEM

  {
    if (fp)
    {
      /* get size */
      fseek(fp,0L,SEEK_END); 
      /* read file possibly partitial */
      if(!*size || *size > ftell(fp))
        *size = ftell (fp);
      rewind(fp);

      DBG_MEM1_S("%u\n",((unsigned int)((size_t)size)));

      if(!*size)
        return mem;

      /* allocate memory */
      oyAllocHelper_m_( mem, char, *size+1, oyAllocateFunc_, return 0);

      /* check and read */
      if ((fp != 0)
       && mem
       && *size)
      {
        int s = fread(mem, sizeof(char), *size, fp);

        DBG_MEM

        /* check again */
        if (s != *size)
        { *size = 0;
          oyFree_m_ (mem)
          mem = 0;
        } else {
          /* copy to external allocator */
          char* temp = mem;
          mem = oyAllocateWrapFunc_( *size+1, allocate_func );
          if(mem) {
            memcpy( mem, temp, *size );
            oyFree_m_ (temp)
            mem[*size] = 0;
          } else {
            oyFree_m_ (mem)
            *size = 0;
          }
        }
      }
    }
  }
 
  DBG_MEM_ENDE
  return mem;
}

char*
oyReadFileToMem_(const char* name, size_t *size,
                 oyAlloc_f     allocate_func)
{
  FILE *fp = 0;
  char* mem = 0;
  const char* filename = name;

  DBG_MEM_START

  DBG_MEM

  if(filename && filename[0] && strlen(filename) > 7 &&
     memcmp(filename, "file://", 7) == 0)
    filename = &filename[7];

  {
    fp = fopen(filename, "rb");
    DBG_MEM2_S ("fp = %u filename = %s\n", (unsigned int)((intptr_t)fp), filename)

    if(fp)
    {
      mem = oyReadFilepToMem_( fp, size, allocate_func );
    } else {
      WARNc2_S( "%s: %s", _("Could not open profile"), oyNoEmptyString_m_(filename) );
    }
  }
 
  /* clean up */
  if (fp) fclose (fp);

  DBG_MEM_ENDE
  return mem;
}

char * oyReadStdinToMem_             ( size_t            * size,
                                       oyAlloc_f           allocate_func )
{
  char * text = 0;
  {
    int text_size = 0, buf_size = 0; 
    int c;
    char * tmp = 0;

    while(((c = getc(stdin)) != EOF))
    {
      if(text_size >= buf_size-1)
      {
        buf_size = text_size + 65536;
        tmp = calloc( sizeof(char), buf_size );
        if(text_size)
          memcpy(tmp, text, text_size);
        free(text);
        text = tmp; tmp = 0;
      }
      text[text_size++] = c;
    }
    if(text)
      text[text_size] = '\000';

    if(size)
      *size = text_size;
  }

  return text;
}

/** @internal
 *  Read a file stream without knowing its size in advance.
 */
char * oyReadUrlToMem_               ( const char        * url,
                                       size_t            * size,
                                       const char        * mode,
                                       oyAlloc_f           allocate_func )
{
  char * text = 0;
  char * command = 0;
  FILE * fp = 0;

  if(url && strlen(url) && size )
  {
    int len = strlen(url), i, pos;
    char * mem = oyAllocateFunc_(len*3+1);
    char c;
    char * app = 0;

    for(i = 0, pos = 0; i < len; ++i)
    {
      c = url[i];
           if(c == ' ')
      { memcpy( &mem[pos], "%20", 3 ); pos += 3; }
      else if(c == '&')
      { memcpy( &mem[pos], "%26", 3 ); pos += 3; }
      else
        mem[pos++] = c;
    }
    mem[pos] = '\000';
    
    if(!app && (app = oyFindApplication( "curl" )) != NULL)
    {
      if(oy_debug)
        oyStringAddPrintf_( &command, oyAllocateFunc_, oyDeAllocateFunc_,
                            "curl -v -s %s", mem );
      else
        oyStringAddPrintf_( &command, oyAllocateFunc_, oyDeAllocateFunc_,
                            "curl -s %s", mem );
    } else if(!app && (app = oyFindApplication( "wget" )) != NULL)
    {
      if(oy_debug)
        oyStringAddPrintf_( &command, oyAllocateFunc_, oyDeAllocateFunc_,
                            "wget -v %s -O -", mem );
      else
        oyStringAddPrintf_( &command, oyAllocateFunc_, oyDeAllocateFunc_,
                            "wget -q %s -O -", mem );
    } else
      WARNc_S(_("Could not download from WWW. Please install curl or wget."));

    if(app) oyFree_m_( app );
  
    if(command)
    {
      if(oy_debug)
        oyMessageFunc_p( oyMSG_DBG, 0, OY_DBG_FORMAT_"%s",OY_DBG_ARGS_, command );
      fp = oyPOPEN_m( command, mode );
    }
    if(fp)
    {
      size_t mem_size = 0;
      char* mem = NULL;

      text = oyReadFileSToMem_(fp, size, allocate_func );

      if(!feof(fp))
      {
        if(text) oyFree_m_(text);
        *size = 0;
        mem_size = 1024;
        mem = malloc(mem_size);
        pclose(fp);
        fp = oyPOPEN_m( command, mode );
      }
      if(fp)
      while(!feof(fp))
      {
        if(*size >= mem_size)
        {
          mem_size *= 10;
          mem = realloc( mem, mem_size );
        }
        *size += fread( &mem[*size], sizeof(char), mem_size-*size, fp );
      }
      if(fp && mem)
      {
          /* copy to external allocator */
          char* temp = mem;
          mem = oyAllocateWrapFunc_( *size+1, allocate_func );
          if(mem) {
            memcpy( mem, temp, *size );
            oyFree_m_ (temp)
            mem[*size] = 0;
          } else {
            oyFree_m_ (mem)
            *size = 0;
          }
        text = mem;
      }
      if(fp)
        pclose(fp);
      fp = 0;
    }
    if(command)
      oyFree_m_(command);
  }

  return text;
}

/** @internal
 *  Read a file stream without knowing its size in advance.
 */
char * oyReadUrlToMemf_              ( size_t            * size,
                                       const char        * mode,
                                       oyAlloc_f           allocate_func,
                                       const char        * format,
                                                           ... )
{
  char * result = NULL;
  char * text = 0;
  va_list list;
  int len;
  size_t sz = 0;

  va_start( list, format);
  len = vsnprintf( text, sz, format, list );
  va_end  ( list );

  if (len >= sz)
  {
    oyAllocHelper_m_(text, char, len + 1, oyAllocateFunc_, return NULL);
    va_start( list, format);
    len = vsnprintf( text, len+1, format, list );
    va_end  ( list );
  }

  result = oyReadUrlToMem_( text, size, mode, allocate_func );

  oyDeAllocateFunc_(text);

  return result;
}

int
oyWriteMemToFile_(const char* name, const void* mem, size_t size)
{
  FILE *fp = 0;
  /*int   pt = 0;
  char* block = mem;*/
  const char* filename;
  char * full_name = 0;
  int r = !name;
  size_t written_n = 0;
  char * path = 0;

  DBG_PROG_START

  DBG_PROG3_S("name = %s mem = %d size = %d\n", name, (int)(intptr_t)mem, (int)(intptr_t)size)

  filename = name;

  if(!r)
  {
    full_name = oyResolveDirFileName_( filename );
    path = oyExtractPathFromFileName_( full_name );
    r = oyMakeDir_( path );
  }

  if(!r)
  {
    fp = fopen(full_name, "wb");
    DBG_PROG2_S("fp = %d filename = %s", (int)(intptr_t)fp, filename)
    if ((fp != 0)
     && mem
     && size)
    {
#if 0
      do {
        r = fputc ( block[pt++] , fp);
      } while (--size);
#else
      written_n = fwrite( mem, 1, size, fp );
      if(written_n != size)
        r = errno;
#endif
    } else 
      if(mem && size)
        r = errno;
      else
        WARNc1_S("no data to write into: \"%s\"", filename );

    if(r)
    {
      switch (errno)
      {
        case EACCES:       WARNc1_S("Permission denied: %s", filename); break;
        case EIO:          WARNc1_S("EIO : %s", filename); break;
#ifdef HAVE_POSIX
        case ELOOP:        WARNc1_S("Too many symbolic links encountered while traversing the path: %s", filename); break;
#endif
        case ENAMETOOLONG: WARNc1_S("ENAMETOOLONG : %s", filename); break;
        case ENOENT:       WARNc1_S("A component of the path/file_name does not exist, or the file_name is an empty string: \"%s\"", filename); break;
        case ENOTDIR:      WARNc1_S("ENOTDIR : %s", filename); break;
#ifdef HAVE_POSIX
        case EOVERFLOW:    WARNc1_S("EOVERFLOW : %s", filename); break;
#endif
      }
    }

    if (fp) fclose (fp);
  }

  if(path) oyDeAllocateFunc_( path );
  if(full_name) oyDeAllocateFunc_( full_name );

  DBG_PROG_ENDE
  return r;
}

char * oyGetTempFileName_            ( const char        * name,
                                       uint32_t            flags,
                                       oyAlloc_f           allocateFunc )
{
  int error = 0;
  const char * filename = name,
             * tmp_dir = 0;
  char * filename_tmp = 0;
  char * full_name = 0;
  int exist = 0, pos = 1;
  char * tmp = 0,
       * result = NULL;

  if(!name)
  {
    filename = tmpnam(NULL);
    flags &= ~OY_FILE_TEMP_DIR;
  }

  if(flags & OY_FILE_TEMP_DIR)
  {
    if(getenv("TMP") && strlen(getenv("TMP")))
      tmp_dir = getenv("TMP");
    else
    if(getenv("TEMP") && strlen(getenv("TEMP")))
      tmp_dir = getenv("TEMP");
    else
    if(getenv("TMPDIR") && strlen(getenv("TMPDIR")))
      tmp_dir = getenv("TMPDIR");
    else
      tmp_dir = "/tmp";

    STRING_ADD( filename_tmp, tmp_dir );
    if(filename_tmp[oyStrlen_(filename_tmp)] != '/')
      STRING_ADD( filename_tmp, "/" );
    STRING_ADD( filename_tmp, filename );
    full_name = filename_tmp;
    filename = full_name;
  }

  if(!full_name)
    full_name = oyResolveDirFileName_( filename );
  exist = oyIsFile_(full_name);
  if(exist &&
     !(flags & OY_FILE_APPEND) && !(flags & OY_FILE_NAME_SEARCH))
  {
    WARNc2_S( "%s: %s", _("File exists"), full_name );
    return NULL;
  }

  if(exist && flags & OY_FILE_NAME_SEARCH && !(flags & OY_FILE_APPEND))
  {
    char * end = 0;
    char * num = 0;
    char * format = oyAllocateFunc_( 32 );
    char * ext = 0;
    int digits = 3;
    int max = 1000;

    /* allocate memory */
    oyAllocHelper_m_( tmp, char, strlen(full_name)+12, oyAllocateFunc_, return NULL);

    oySprintf_( tmp, "%s", full_name );
    ext = end = oyStrrchr_( tmp, '.' );
    if(!end)
      end = tmp + oyStrlen_( tmp );
    num = end;
    while(isdigit( num[0] ) && num != tmp) --num;
    if(end - num)
      digits = (int) (end - num);
    /* move the extension */
    else if(ext)
      memmove( &ext[digits], ext, digits + 1 );
    /* settle with a new format */
    sprintf( format, "%%0%dd", digits );
    pos += atoi(num);
    /* write a new number */
    sprintf( num, format, pos );
    if(ext)
      tmp[oyStrlen_(tmp)] = '.';

    /* search a non existing file name */
    while(oyIsFile_(tmp))
    {
      sprintf( num, format, pos++ );
      if(ext)
        tmp[oyStrlen_(tmp)] = '.';
    }
    max = (int)pow( 10, digits);
    if(pos >= max)
    {
      WARNc2_S( "%s: %s", _("File exists"), tmp );
      return NULL;
    }

    filename = tmp;
    if(format) oyFree_m_(format)
  }

  if(!error)
  {
    result = oyStringCopy_( filename,
                            allocateFunc?allocateFunc:oyAllocateFunc_ );
  }

  if(tmp) oyFree_m_(tmp)

  return result;
}
 

/* TODO: support flags - OY_FILE_APPEND */
int  oyWriteMemToFile2_              ( const char        * name,
                                       void              * mem,
                                       size_t              size,
                                       uint32_t            flags,
                                       char             ** result,
                                       oyAlloc_f           allocateFunc )
{
  int error = 0;
  char * filename = NULL;

  if(!name)
    return 1;

  filename = oyGetTempFileName_( name, flags, allocateFunc );

  error = !filename;

  if(!error)
  {
    error = oyWriteMemToFile_( filename, mem, size );
    *result = filename;
  }

  return error;
}

char*
oyGetHomeDir_ ()
{
# if (__WINDOWS__)
  DBG_PROG_START
  DBG_PROG_ENDE
  WARNc_S("OS not supported yet");
  return 0;
# else
  char* name = (char*) getenv("HOME");

  DBG_PROG_START

  if(!name)
    WARNc_S("Could not get \"HOME\" directory name");

  DBG_PROG_ENDE
  return name;
# endif
}

char*
oyPathGetParent_ (const char* name)
{
  char *parentDir = 0, *ptr = 0;

  DBG_PROG_START

  oyAllocString_m_( parentDir, MAX_PATH,
                    oyAllocateFunc_, return 0 );

  oySprintf_ (parentDir, "%s", name);
  ptr = strrchr( parentDir, OY_SLASH_C);
  if (ptr)
  {
    if (ptr[1] == 0) /* ending dir separator */
    {
      ptr[0] = 0;
      if (strrchr( parentDir, OY_SLASH_C))
      {
        ptr = strrchr (parentDir, OY_SLASH_C);
        ptr[0+1] = 0;
      }
    }
    else
      ptr[0+1] = 0;
  }

  DBG_PROG_S(parentDir)

  DBG_PROG_ENDE
  return parentDir;
}

int oyIsDirFull_ (const char* name)
{
  struct stat status;
  int r = 0;

  DBG_MEM_START

  status.st_mode = 0;
  r = stat (name, &status);
  DBG_MEM1_S("status.st_mode = %d", (int)(status.st_mode&S_IFMT)&S_IFDIR)
  DBG_MEM1_S("status.st_mode = %d", (int)status.st_mode)
  DBG_MEM1_S("name = %s ", name)
  r = !r &&
       ((status.st_mode & S_IFMT) & S_IFDIR);

  DBG_MEM_ENDE
  return r;
}

int oyIsDir_ (const char* path)
{
  int r = 0;
  DBG_MEM_START

  char* name = oyResolveDirFileName_ (path);

  r = oyIsDirFull_(name);

  oyFree_m_(name)

  DBG_MEM_ENDE
  return r;
}

int
oyIsFileFull_ (const char* fullFileName, const char * read_mode)
{
  struct stat status;
  int r = 0;
  const char* name = fullFileName;

  DBG_MEM_START

  DBG_MEM1_S("fullFileName = \"%s\"", fullFileName)
  status.st_mode = 0;
  r = stat (name, &status);

  DBG_MEM1_S("status.st_mode = %d", (int)(status.st_mode&S_IFMT)&S_IFDIR)
  DBG_MEM1_S("status.st_mode = %d", (int)status.st_mode)
  DBG_MEM1_S("name = %s", name)
  DBG_MEM_V( r )
  switch (r)
  {
    case EACCES:       WARNc1_S("EACCES = %d\n",r); break;
    case EIO:          WARNc1_S("EIO = %d\n",r); break;
#ifdef HAVE_POSIX
    case ELOOP:        WARNc1_S("ELOOP = %d\n",r); break;
#endif
    case ENAMETOOLONG: WARNc1_S("ENAMETOOLONG = %d\n",r); break;
    case ENOENT:       WARNc1_S("ENOENT = %d\n",r); break;
    case ENOTDIR:      WARNc1_S("ENOTDIR = %d\n",r); break;
#ifdef HAVE_POSIX
    case EOVERFLOW:    WARNc1_S("EOVERFLOW = %d\n",r); break;
#endif
  }

  r = !r &&
       (   ((status.st_mode & S_IFMT) & S_IFREG)
#ifdef HAVE_POSIX
        || ((status.st_mode & S_IFMT) & S_IFLNK)
#endif
                                                );

  DBG_MEM_V( r )
  if (r)
  {
    FILE* fp = fopen (name, read_mode);
    if (!fp) { DBG_PROG1_S("not existent: %s", name )
      r = 0;
    } else { DBG_MEM_S(name)
      fclose (fp);
    }
  }

  DBG_MEM_ENDE
  return r;
}

int
oyIsFile_ (const char* fileName)
{
  int r = 0;
  char* name = oyResolveDirFileName_ (fileName);

  DBG_MEM_START

  r = oyIsFileFull_(name,"rb");

  oyFree_m_ (name) DBG_MEM

  DBG_MEM_ENDE
  return r;
}

int
oyMakeDir_ (const char* path)
{
  char * full_name = oyResolveDirFileName_ (path),
       * path_parent = 0,
       * path_name = 0;
  int rc = !full_name;

#ifdef HAVE_POSIX
  mode_t mode = S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH; /* 0755 */
#endif

  DBG_PROG_START


  if(full_name)
    path_name = oyExtractPathFromFileName_(full_name);
  if(path_name)
  {
    if(!oyIsDir_(path_name))
    {
      path_parent = oyPathGetParent_(path_name);
      if(!oyIsDir_(path_parent))
      {
        rc = oyMakeDir_(path_parent);
        oyDeAllocateFunc_( path_parent );
      }

      rc = mkdir (path_name
#ifdef HAVE_POSIX
                            , mode
#endif
                                  );
      if(rc)
      switch (errno)
      {
        case EACCES:       WARNc1_S("Permission denied: %s", path); break;
        case EIO:          WARNc1_S("EIO : %s", path); break;
#ifdef HAVE_POSIX
        case ELOOP:        WARNc1_S("Too many symbolic links encountered while traversing the path: %s", path); break;
#endif
        case ENAMETOOLONG: WARNc1_S("ENAMETOOLONG : %s", path); break;
        case ENOENT:       WARNc1_S("A component of the path/file_name does not exist, or the file_name is an empty string: \"%s\"", path); break;
        case ENOTDIR:      WARNc1_S("ENOTDIR : %s", path); break;
#ifdef HAVE_POSIX
        case EOVERFLOW:    WARNc1_S("EOVERFLOW : %s", path); break;
#endif
      }
    }
    oyDeAllocateFunc_( path_name );;
  }

  oyFree_m_ (full_name)

  DBG_PROG_ENDE
  return rc;
}

int  oyRemoveFile_                   ( const char        * full_file_name )
{
  return remove( full_file_name );
}

char*
oyResolveDirFileName_ (const char* name)
{
  char * newName = NULL,
       * home = NULL;
  int len = 0;

  DBG_MEM_START

  DBG_MEM_S(name)

  /* user directory */
  if (name[0] == '~')
  {
    home = oyGetHomeDir_();
    len = strlen(name) + strlen(home) + 1;
    oyAllocHelper_m_( newName, char, len + 2, oyAllocateFunc_, fprintf(stderr,"oyranos_io.c:367 oyResolveDirFileName_() Could not allocate enough memory.\n"); return 0 );

    oySprintf_ (newName, "%s%s", home, &name[0]+1);

  } else {
    len = strlen(name)+1;
    oyAllocHelper_m_( newName, char, len, oyAllocateFunc_, fprintf(stderr,"oyranos_io.c:554 oyResolveDirFileName_() Could not allocate enough memory.\n"); return 0 );
    oySprintf_ (newName, "%s", name);

    /* relative names - where the first sign is no directory separator */
    if (newName[0] != OY_SLASH_C)
    {
      char* cn = 0;
      const char * pw = getenv("PWD");

      len += strlen(pw) + 10;

      STRING_ADD(cn, pw);
      STRING_ADD(cn, OY_SLASH);
      STRING_ADD(cn, name);
      DBG_MEM1_S("canonoical %s ", cn)
      oyFree_m_(newName);
      STRING_ADD(newName, cn);
      oyFree_m_(cn);
    }
  }

  if(name)
    DBG_MEM1_S ("name %s", name);
  if(home)
    DBG_MEM1_S ("home %s", home);
  if(newName)
    DBG_MEM1_S ("newName = %s", newName);

  DBG_MEM_ENDE
  return newName;
}

char*
oyExtractPathFromFileName_ (const char* file_name)
{
  char *path_name = 0;
  char *ptr;

  DBG_MEM_START

  oyAllocString_m_( path_name, strlen(file_name)+1,
                    oyAllocateFunc_, return 0 );

  oySprintf_( path_name, "%s", file_name );
  DBG_MEM1_S ("path_name = %s", path_name)
  ptr = strrchr (path_name, '/');
  ptr[0+1] = 0;
  DBG_MEM1_S ("path_name = %s", path_name)
  DBG_MEM1_S ("ptr = %s", ptr)
  DBG_MEM_ENDE
  return path_name;
}

char*
oyMakeFullFileDirName_ (const char* name)
{
  char *newName = 0;
  char *dirName = 0;

  DBG_MEM_START

  DBG_MEM
  if(name &&
     strrchr( name, OY_SLASH_C ))
  { DBG_MEM
    /* substitute ~ with HOME variable from environment */
    newName = oyResolveDirFileName_ (name);
  } else
  { DBG_MEM
    /* create directory name */
    oyAllocString_m_( newName, MAX_PATH,
                      oyAllocateFunc_, return 0 );
    dirName = (char*) getenv("PWD");
    oySprintf_ (newName, "%s%s", dirName, OY_SLASH);
    if (name)
      oySprintf_ (strrchr(newName,OY_SLASH_C)+1, "%s", name);
    DBG_MEM1_S("newName = %s", newName)
  }

  DBG_MEM1_S("newName = %s", newName)

  DBG_MEM_ENDE
  return newName;
}

/* resembles which */
char * oyFindApplication(const char * app_name)
{
  const char * path = getenv("PATH");
  char * full_app_name = NULL;
  if(path && app_name)
  {
    int paths_n = 0, i;
    char ** paths = oyStringSplit_( path, ':', &paths_n, malloc );
    for(i = 0; i < paths_n; ++i)
    {
      char * full_name = 0;
      int found;
      STRING_ADD( full_name, paths[i] );
      STRING_ADD( full_name, OY_SLASH );
      STRING_ADD( full_name, app_name );
      found = oyIsFileFull_( full_name, "rb" );
      if(found)
      {
        i = paths_n;
        full_app_name = strdup( full_name );
      }
      oyFree_m_( full_name );
    }
    oyStringListRelease_( &paths, paths_n, free );
  }
  return full_app_name;
}


typedef int (*pathSelect_f_)(oyFileList_s*,const char*,const char*);

int oyStrcmpWrap( const void * a, const void * b)
{
  const char ** ca = (const char**)a, ** cb = (const char**)b;
  const char * ca_ = *ca, *cb_ = *cb;
  if(strrchr(ca_, OY_SLASH_C))
    ca_ = strrchr(ca_, OY_SLASH_C);
  if(strrchr(cb_, OY_SLASH_C))
    cb_ = strrchr(cb_, OY_SLASH_C);
  return strcmp(ca_,cb_);
}

#define MAX_DEPTH 64


/** @internal
 *  doInPath and data must fit, doInPath can operate on data and after finishing
 *  oyRecursivePaths_ data can be processed further

 * TODO: move specifying paths out as arguments

 */
int
oyRecursivePaths_  ( pathSelect_f_ doInPath,
                     oyFileList_s * data,
                     const char ** path_names,
                     int count )
{
  int r = 0;
  int i;

  static int war = 0;

  DBG_MEM_START

  ++war;
  if(war >= 413)
    { ;/* WARNc_S(("schon %d mal\n", war)); */ }

  for(i = 0; i < count ; ++i)
  {
    struct stat statbuf;
    struct dirent *entry[MAX_DEPTH];
    DIR *dir[MAX_DEPTH];
    const char *path = path_names[i];
    int l = 0; /* level */
    int run = !r;
    int path_is_double = 0;
    int j;

    /* check for doubling of paths, without checking recursively */
    {
      for( j = 0; j < i; ++j )
      { const char *p  = path_names[j];
        char *pp = oyMakeFullFileDirName_( p );
        if( p && pp &&
            (strcmp( path, pp ) == 0) )
          path_is_double = 1;
        oyFree_m_( pp );
      }
      memset(dir, 0, sizeof(DIR*) * MAX_DEPTH); 
      memset(entry, 0, sizeof(struct dirent*) * MAX_DEPTH);
    }

    if( path_is_double )
      continue;

    if ((stat (path, &statbuf)) != 0) {
      switch (errno)
      {
        case EACCES:       WARNc2_S("Permission denied: %s %d", path, i); break;
        case EIO:          WARNc2_S("EIO : %s %d", path, i); break;
#ifdef HAVE_POSIX
        case ELOOP:        WARNc2_S("Too many symbolic links encountered while traversing the path: %s %d", path, i); break;
#endif
        case ENAMETOOLONG: WARNc2_S("ENAMETOOLONG : %s %d", path, i); break;
        case ENOENT:       DBG_MEM2_S("A component of the path file_name does not exist, or the path is an empty string: \"%s\" %d", path, i); break;
        case ENOTDIR:      WARNc2_S("ENOTDIR : %s %d", path, i); break;
#ifdef HAVE_POSIX
        case EOVERFLOW:    WARNc2_S("EOVERFLOW : %s %d", path, i); break;
#endif
      }
      continue;
    }
    if (!S_ISDIR (statbuf.st_mode)) {
      WARNc3_S("%d. \"%s\" %s", i, path, _("path is not a directory"));
      continue;
    }
#ifdef HAVE_POSIX
    if (S_ISLNK (statbuf.st_mode)) {
      WARNc3_S("%d. \"%s\" %s", i, path, _("path is a link: ignored"));
      continue;
    }
#endif
    dir[l] = opendir (path);
    if (!dir[l]) {
      WARNc3_S("%d. \"%s\" %s", i, path, _("path is not readable"));
      continue;
    }

    while(run)
    {
      char name[256];
      int k;

      if(l>=64) WARNc_S("max path depth reached: 64");
      if(dir[l] == NULL)
      {
        WARNc_S("NULL");
        --l;
        if(l<0) run = 0;
        goto cont;
      }
      if(!(entry[l] = readdir (dir[l]))) {
        closedir(dir[l]);
        dir[l] = NULL;
        --l;
        if(l<0)
          run = 0;
        goto cont;
      }

      oySprintf_(name, "%s",path);
      for (k=0; k <= l; ++k) {
        int len = strlen(name);
        assert(entry[k] && entry[k]->d_name);
        if(len+strlen(entry[k]->d_name) < 256)
          oySprintf_(&name[strlen(name)],"/%s", entry[k]->d_name);
        else {
          DBG_MEM3_S("%d. %s/%s ignored", l, name, entry[k]->d_name)
          goto cont;
        }
      }

      if ((strcmp (entry[l]->d_name, "..") == 0) ||
          (strcmp (entry[l]->d_name, ".") == 0)) {
        DBG_MEM2_S("%d. %s ignored", l, name)
        goto cont;
      }
      if ((stat (name, &statbuf)) != 0) {
        DBG_MEM2_S("%d. %s does not exist", l, name)
        goto cont;
      }
#ifdef HAVE_POSIX
      if (!S_ISLNK(statbuf.st_mode)){/*((statbuf.st_mode & S_IFMT) & S_IFLNK))  */
        DBG_MEM5_S("%d. %s is a link: ignored %d %d %d", l, name, (int)statbuf.st_mode , S_IFLNK, 0120000);
        /*goto cont; */
      }
#endif
      if (S_ISDIR (statbuf.st_mode) &&
          l < MAX_DEPTH ) {

        dir[l+1] = opendir (name);
        ++l;
        DBG_MEM2_S("%d. %s directory", l, name);
        goto cont;
      }
      if(!S_ISREG (statbuf.st_mode)) {
        DBG_MEM2_S("%d. %s is a non regular file", l, name);
        goto cont;
      }
      DBG_MEM2_S( "%d. a valid file %s", l, name )

      /* use all file extensions */
      /* go recursively without following links, due to security */
      if( !r ) {
        r = doInPath(data, name, entry[l]->d_name);
        run = !r;
        if(r)
          DBG_MEM3_S("%d. %d %d found", i, r, run);
      }

      cont:
        ;
    }

    for( j = 0; j < MAX_DEPTH; ++j ) { if(dir[j]) closedir(dir[j]); dir[j] = NULL; }

    qsort( data->names, data->count_files, sizeof(char*),
           oyStrcmpWrap );
  }

  DBG_MEM_ENDE
  return r;
}

int oyFileListCb_ ( oyFileList_s * data,
                    const char * full_name, const char * filename)
{
  oyFileList_s *l = (oyFileList_s*)data;

  if(l->type != oyOBJECT_FILE_LIST_S_)
    WARNc_S("Could not find a oyFileList_s objetc.");

  {
        if(l->count_files >= l->mem_count)
        {
          char** temp = l->names;

          l->names = 0;
          oyAllocHelper_m_( l->names, char*, l->mem_count+l->hopp,
                            oyAllocateFunc_, return 1);
          memcpy(l->names, temp, sizeof(char*) * l->mem_count);
          l->mem_count += l->hopp;
        }

        oyAllocString_m_( l->names[l->count_files], oyStrblen_(full_name),
                          oyAllocateFunc_, return 1 );
        
        oyStrcpy_(l->names[l->count_files], full_name);
        ++l->count_files;
  }

  return 0;
}


/** @internal
 *  @brief query valid XDG paths
 *
 *  @see Directory paths for color profiles and other configuration data
 *       http://www.oyranos.com/wiki/index.php?title=OpenIccDirectoryProposal
 *
 *  @param[out]    count       number of paths found
 *  @param[in]     data        oyYES/oyNO/oyALL for data or config text
 *  @param[in]     owner       oyUSER/oySYS/oyUSER_SYS
 *  @return                    a array to write the found paths into
 *
 *  @since Oyranos: version 0.1.8
 *  @date  november 2007 (API 0.1.8)
 */
char**  oyXDGPathsGet_( int             * count,
                        int               data,
                        int               owner,
                        oyAlloc_f         allocateFunc )
{
  char ** paths = 0, ** tmp;
  int     n = 0, tmp_n = 0;
  char *  vars[] = {"XDG_DATA_HOME", "XDG_CONFIG_HOME", "XDG_DATA_DIRS", 
                    "XDG_CONFIG_DIRS"};
  int     vars_n = 4;
  int     i, j;

  for(i = 0; i < vars_n; ++i)
  {
    if( (i >= 2 && (owner != oyUSER)) ||
        (i < 2 && (owner != oySYS)))
      if( ((i == 0 || i == 2) && data != oyNO) ||
          ((i == 1 || i == 3) && data != oyYES) )
    {
      const char * var = getenv(vars[i]);
      if(var)
      {
        if(strlen(var))
        {
          char **tmp_neu;
          int  tmp_neu_n;


          tmp = oyStringSplit_( var, ':', &tmp_n, oyAllocateFunc_ );

          /* remove slash */
          for(j = 0; j < tmp_n; ++j)
          {
            char slash = 0;
            int len = 0;
            if(tmp[j])
              len = oyStrlen_(tmp[j]);
            if(len > 1)
              slash = tmp[j][len-1];
            if(slash == OY_SLASH_C)
              tmp[j][oyStrlen_(tmp[j])-1] = 0;
          }

          tmp_neu = oyStringListAppend_( (const char**)paths, n, 
                                         (const char**)tmp, tmp_n,
                                         &tmp_neu_n, oyAllocateFunc_ );
          oyStringListRelease_( &paths, n, oyDeAllocateFunc_ );
          oyStringListRelease_( &tmp, tmp_n, oyDeAllocateFunc_ );
          paths = tmp_neu;
          n = tmp_neu_n;
        }
      } else
      {
        if(i == 0)
          oyStringListAddStaticString_( &paths, &n, "~/.local/share",
                                        oyAllocateFunc_, oyDeAllocateFunc_ );
        if(i == 1)
          oyStringListAddStaticString_( &paths, &n, "~/.config",
                                        oyAllocateFunc_, oyDeAllocateFunc_ );
        if(i == 4)
          oyStringListAddStaticString_( &paths, &n, "/etc/xdg",
                                        oyAllocateFunc_, oyDeAllocateFunc_ );
      }

      /* add the OpenIccDirectoryProposal default paths */
      if(i == 0)
      {
          oyStringListAddStaticString_( &paths, &n, "/usr/share",
                                        oyAllocateFunc_, oyDeAllocateFunc_ );
          oyStringListAddStaticString_( &paths, &n, "/usr/local/share",
                                        oyAllocateFunc_, oyDeAllocateFunc_ );
          oyStringListAddStaticString_( &paths, &n, "/var/lib",
                                        oyAllocateFunc_, oyDeAllocateFunc_ );
      }
    }
  }

  tmp = oyStringListAppend_( 0, 0, (const char**)paths, n,
                             &tmp_n, allocateFunc );
  oyStringListRelease_( &paths, n, oyDeAllocateFunc_ );
  paths = tmp;
  n = tmp_n;

  if(count)
    *count = n;

  if(!n)
  {
    if(paths)
      oyDeAllocateFunc_(paths);
    paths = 0;
  }

  return paths;
}

char * oyPathContructAndTest_(char * path_, const char * subdir)
{
  char * text = 0, * tmp = 0;

  if(!path_)
    return 0;

  STRING_ADD( text, path_ );
  if(subdir)
  {
     STRING_ADD( text, OY_SLASH );
     STRING_ADD( text, subdir );
  }

  tmp = oyResolveDirFileName_( text );
  oyDeAllocateFunc_(text); text = tmp; tmp = 0;

  if(!oyIsDir_( text ))
  {
    oyDeAllocateFunc_(text);
    text = 0;
  }

  return text; 
}


/** @internal
 *  @brief query valid Oyranos paths
 *
 *  @param[out]    count       number of paths found
 *  @param[in]     subdir      the data directories sub path, a string 
 *                             containing one path, e.g. "color/icc""
 *  @param[in]     data        oyYES/oyNO/oyALL data or config text
 *  @param[in]     owner       oyUSER/oySYS/oyUSER_SYS
 *
 *  @version Oyranos: 0.1.11
 *  @date    2010/08/18
 *  @since   2007/11/00 (Oyranos: 0.1.x)
 */
char**
oyDataPathsGet_       (int             * count,
                       const char      * subdir,
                       int               data,
                       int               owner,
                       oyAlloc_f         allocateFunc )
{
  char ** paths = NULL;
  int ndp = 0;        /* number of default paths */

  /* the OpenICC agreed upon *nix default paths */
  {
    int xdg_n = 0, oy_n = 0, tmp_n = 0, i,j,has;
    char ** oy_paths = 0;
    char ** xdg_paths = 0;
    char ** tmp_paths = 0;
    char * text = 0;
    char * xdg_sub = 0, * x = 0;

    oyAllocHelper_m_( xdg_sub, char, MAX_PATH, oyAllocateFunc_, return 0);
    oySprintf_( xdg_sub, "%s", subdir );

    oyAllocHelper_m_( oy_paths, char*, 6, oyAllocateFunc_, return 0);

    xdg_paths = oyXDGPathsGet_(&xdg_n, data, owner, oyAllocateFunc_);

    if(xdg_n)
      oyAllocHelper_m_( tmp_paths, char*, xdg_n, oyAllocateFunc_, return 0);

    for(i = 0; i < xdg_n; ++i)
    {
      x = xdg_paths[i];
      text = oyPathContructAndTest_(x, xdg_sub);
      if(text)
        tmp_paths[tmp_n++] = text;
    }

    oyStringListRelease_(&xdg_paths, xdg_n, oyDeAllocateFunc_);
    xdg_paths = tmp_paths; tmp_paths = 0;
    xdg_n = tmp_n; tmp_n = 0;
    oyFree_m_(xdg_sub);

    ndp += xdg_n;

    text = oyPathContructAndTest_( "/usr/share", subdir );
    if(text) oy_paths[oy_n++] = text;
    text = oyPathContructAndTest_( "/usr/local/share", subdir );
    if(text) oy_paths[oy_n++] = text;
    text = oyPathContructAndTest_( "/var/lib", subdir );
    if(text) oy_paths[oy_n++] = text;
    text = 0;
    if(subdir && strlen(subdir) > 6 && memcmp( subdir,"color/", 6 ) == 0)
    text = oyPathContructAndTest_( OY_USERCOLORDATA, &subdir[6] );
    if(text) oy_paths[oy_n++] = text;
    if(oyIsDir_( OY_PROFILE_PATH_USER_DEFAULT ))
    {
      text = oyResolveDirFileName_(OY_PROFILE_PATH_USER_DEFAULT);
      if(text) oy_paths[oy_n++] = text;
    }
    text = oyPathContructAndTest_( OY_DATADIR, subdir );
    if(text) oy_paths[oy_n++] = text;

    paths = oyStringListAppend_(0,0, (const char**)oy_paths, oy_n,
                                &ndp, allocateFunc);
    for(i = 0; i < xdg_n; ++i)
    {
      has = 0;
      text = xdg_paths[i];

      for(j = 0; j < oy_n; ++j)
        if(text && oyStrcmp_( text, oy_paths[j] ) == 0)
        {
          has = 1;
          break;
        }

      if(!has)
        oyStringListAddStaticString_( &paths, &ndp, text,
                                      oyAllocateFunc_, oyDeAllocateFunc_ );
    }

    oyStringListRelease_(&oy_paths, oy_n, oyDeAllocateFunc_);
    oyStringListRelease_(&xdg_paths, xdg_n, oyDeAllocateFunc_);

  }

  *count = ndp;

  return paths;
}

/** @internal
 *  @brief get files out of the Oyranos default paths
 *
 *  Since: 0.1.8
 */
char**
oyFileListGet_                  (const char * subpath,
                                 int        * size,
                                 int          data,
                                 int          owner)
{
  oyFileList_s l = {oyOBJECT_FILE_LIST_S_, 128, NULL, 0, 128, 0, 0};
  int count = 0;
  char ** path_names = NULL;
 
  DBG_PROG_START
 
  path_names = oyDataPathsGet_( &count, subpath, oyALL, oyUSER_SYS,
                                oyAllocateFunc_ );

  oy_warn_ = 0;

  l.names = 0;
  l.mem_count = l.hopp;
  l.count_files = 0;

  oyAllocHelper_m_(l.names, char*, l.mem_count, oyAllocateFunc_, return 0);

  oyRecursivePaths_(oyFileListCb_, &l, (const char**)path_names, count);

  oyStringListRelease_(&path_names, count, oyDeAllocateFunc_);

  *size = l.count_files;

  if(!l.count_files && l.names)
  {
    oyDeAllocateFunc_(l.names);
    l.names = 0;
  }

  oy_warn_ = 1;
  DBG_PROG_ENDE
  return l.names;
}



/* Oyranos text handling */

/** @internal
 *  @brief query library paths
 *
 *  @param[out]    count       number of paths found
 *  @param[in]     subdir      sub path; use "" to skip, otherwise OY_METASUBPATH is assumed; optional
 *  @param[in]     owner       oyUSER/oySYS/oyUSER_SYS
 *  @param[in]     allocateFunc  user specified allocator
 *  @return                    a array to write the found paths into
 *
 *  @version Oyranos: 0.9.5
 *  @date    2014/01/02
 *  @since   2007/11/00 (Oyranos: 0.1.8)
 */
char**  oyLibPathsGet_( int             * count,
                        const char      * subdir,
                        int               owner,
                        oyAlloc_f         allocateFunc )
{
  char ** paths = 0, ** tmp;
  int     n = 0, tmp_n = 0;
  char *  vars[] = {OY_MODULE_PATH,"LD_LIBRARY_PATH"};
  int     vars_n = 2;
  int     i,j;
  char  * fix_paths[5] = {0,0,0,0,0};
  int     fix_paths_n = 2;
  char  * full_path = 0, * fp = NULL;
  const char * lib = "lib";

  /* guesswork */
  if(strstr(OY_LIBDIR,"lib64") != NULL)
    lib = "lib64";

  if(!subdir)
  {
    full_path = oyResolveDirFileName_( OY_LIBDIR OY_SLASH OY_METASUBPATH );
    fix_paths[0] = full_path;
    oyStringAddPrintf_( &fp, AD, "%s%s%s", OY_USER_PATH OY_SLASH, lib, OY_SLASH OY_METASUBPATH );
    full_path = oyResolveDirFileName_( fp );
    fix_paths[1] = full_path;
    oyFree_m_(fp);

    subdir = OY_METASUBPATH;
  } else {
    full_path = oyResolveDirFileName_( OY_LIBDIR OY_SLASH );
    STRING_ADD( fix_paths[0], full_path );
    oyFree_m_( full_path );
    STRING_ADD( fix_paths[0], subdir );

    oyStringAddPrintf_( &fp, AD, "%s%s%s", OY_USER_PATH OY_SLASH, lib, OY_SLASH );
    full_path = oyResolveDirFileName_( fp );
    oyFree_m_(fp);
    STRING_ADD( fix_paths[1], full_path );
    oyFree_m_( full_path );
    STRING_ADD( fix_paths[1], subdir );
  }

  oyStringAddPrintf_( &fp, AD, "/usr/" "%s" OY_SLASH "%s", lib, subdir );
  fix_paths[fix_paths_n++] = oyResolveDirFileName_( fp );
  oyFree_m_(fp);

  oyStringAddPrintf_( &fp, AD, "/usr/local/" "%s" OY_SLASH "%s", lib, subdir );
  fix_paths[fix_paths_n++] = oyResolveDirFileName_( fp );
  oyFree_m_(fp);

  oyStringListAdd_( &paths, &n, (const char**)fix_paths, fix_paths_n,
                    oyAllocateFunc_, oyDeAllocateFunc_ );

  for(i = 0; i < vars_n; ++i)
  {
    if( (i >= 2 && (owner != oyUSER)) ||
        (i < 2 && (owner != oySYS)))
    {
      const char * var = getenv(vars[i]);
      if(var)
      {

        if(strlen(var))
        {
          char **tmp_neu,
               **full_paths;
          int  tmp_neu_n, full_paths_n = 0;

          tmp = oyStringSplit_( var, ':', &tmp_n, oyAllocateFunc_ );

          full_paths = oyAllocateFunc_(sizeof(char*) * (tmp_n + 1));
          for(j = 0; j < tmp_n; ++j)
          {
            char * full_name = oyResolveDirFileName_( tmp[j] );
            oyStringAddPrintf_( &fp, AD, "%s" OY_SLASH "%s", full_name, subdir );
            if(!oyStringListHas_((const char**)paths,n,fp))
            {
              if(!oyIsDir_(fp))
                WARNc4_S("%s %s:\"%s\"/\"%s\"",_("path is not readable"), vars[i], full_name, subdir );
              full_paths[full_paths_n++] = fp; fp = NULL;
            } else
              oyFree_m_( fp );
            oyFree_m_( full_name );
          }
          tmp_neu = oyStringListAppend_( (const char**)paths, n,
                                         (const char**)full_paths, full_paths_n,
                                         &tmp_neu_n, oyAllocateFunc_ );
          oyStringListRelease_( &paths, n, oyDeAllocateFunc_ );
          oyStringListRelease_( &full_paths, full_paths_n, oyDeAllocateFunc_ );
          paths = tmp_neu;
          n = tmp_neu_n;
        }
      }
    }
  }

  tmp = oyStringListAppend_( 0, 0, (const char**)paths, n,
                             &tmp_n, allocateFunc );
  oyStringListRelease_( &paths, n, oyDeAllocateFunc_ );
  paths = tmp;
  n = tmp_n;

  for(i = 0; i < fix_paths_n; ++i)
    if(fix_paths[i])
      oyFree_m_(fix_paths[i]);

  if(count)
    *count = n;

  if(!n)
  {
    if(paths)
      oyDeAllocateFunc_(paths);
    paths = 0;
  }

  return paths;
}

/** @internal
 *  @brief get files out of the Oyranos default paths
 *
 *  Since: 0.1.8
 */
char**
oyLibListGet_                   (const char * subpath,
                                 int        * size,
                                 int          owner)
{
  struct oyFileList_s l = {oyOBJECT_FILE_LIST_S_, 128, NULL, 0, 128, 0, 0};
  int count = 0;
  char ** path_names = NULL;
 
  DBG_PROG_START
 
  path_names = oyLibPathsGet_( &count, subpath, oyUSER_SYS,
                                  oyAllocateFunc_ );

  oy_warn_ = 0;

  l.names = 0;
  l.mem_count = l.hopp;
  l.count_files = 0;

  oyAllocHelper_m_(l.names, char*, l.mem_count, oyAllocateFunc_, return 0);

  oyRecursivePaths_(oyFileListCb_, &l, (const char**)path_names, count);

  oyStringListRelease_(&path_names, count, oyDeAllocateFunc_);

  *size = l.count_files;
  oy_warn_ = 1;
  DBG_PROG_ENDE
  return l.names;
}

/** @internal
 *  @brief construct library name for dlopen'ing
 *
 *  Since: 0.9.5
 */
char*   oyLibNameCreate_                 ( const char * lib_base_name,
                                           int          version )
{
  char * fn = NULL;

#ifdef __APPLE__
    oyStringAddPrintf_( &fn, oyAllocateFunc_, oyDeAllocateFunc_,
                        "%s%s.%d%s", OY_LIB_PREFIX, lib_base_name, version, OY_LIB_SUFFIX );
#else
    oyStringAddPrintf_( &fn, oyAllocateFunc_, oyDeAllocateFunc_,
                        "%s%s%s.%d", OY_LIB_PREFIX, lib_base_name, OY_LIB_SUFFIX, version );
#endif
  return fn;  
}
