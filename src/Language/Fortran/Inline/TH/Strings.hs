{-|
Module      : Language.Rust.Inline
Description : Quasiquotes for writing Rust code inline in Haskell
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Fortran.Inline.TH.Strings
  where

import Text.RawString.QQ

sourceC :: String
sourceC = [r|
# define BIG 4096

# ifndef TRUE
#   define TRUE 1
# endif

# ifndef FALSE
#   define FALSE 0
# endif

# define NAMELENGTH 31

int blkdatano = 0;

int main_f90split ( char *argv );
int compare ( char **s0, char *t );
int endcard ( char *s );
void get_name ( char *s, char *f );
void split_file ( FILE *file_input );
size_t sgets (char *inBuf, size_t n, char *outBuf);

size_t sgets (char *inBuf, size_t n, char *outBuf)
{
    size_t len = 0;
    unsigned char *s;
    unsigned char *p, *t;

    if (n <= 0)             /* sanity check */
            return (-1);

    p =  inBuf;
    s = outBuf;

    n--;                    /* leave space for NUL */

    while (n != 0) {

        len = n;
        t = memchr((void *)p, '\n', strlen(p));

        //printf ("'p' found at position %d.\n", t -p + 1);

        if (t != NULL) {
            len = ++t -p;
            (void)memcpy((void *)s, (void *)p, len);
            s[len] = 0;
            return len;
        }

        (void)memcpy((void *)s, (void *)p, len);
        s += len;
        n -= len;

    }

    *s = 0;

    return len;

}

/******************************************************************************/

int main_f90split ( char *argv )

/******************************************************************************/
/*
  Purpose:

    MAIN is the main program for F90SPLIT.

  Discussion:

    F90SPLIT splits a file of FORTRAN90 modules into individual files.

  Invocation:

    f90split files

  Discussion:

    Procedure X is put in file X.f90.

    Comments preceding a procedure, and not associated with a
    preceding procedure, are included with that procedure.

  Concerns:

    You cannot use the more elaborate END statements that include the
    procedure definition, such as "END FUNCTION ALPHA".

  Modified:

    05 December 2007

*/
{
  FILE *file_input;

    printf ( "Splitting %s.\n", argv );

    file_input = fopen ( argv, "r" );

    if ( file_input == NULL )
    {
      fprintf ( stderr, "\n" );
      fprintf ( stderr, "FSPLIT90: Error!\n" );
      fprintf ( stderr, "  Cannot open %s\n", argv );
      exit;
    }

    split_file ( file_input );

    fclose ( file_input );

  return 0;
}
/******************************************************************************/

int compare ( char **s0, char *t )

/******************************************************************************/
/*
  Purpose:

    COMPARE compares two strings for equality.

  Discussion:

    Assume that t is all lower case.
    Ignore blanks and decase s during comparison.

  Modified:

    05 December 2007

  Parameters:

    S0 points to next character after successful comparison.
*/
{
  char *s;
  int s1;

  s = *s0;

  while ( *t )
  {
    while ( isspace ( *s ) )
    {
      ++s;
    }
    s1 = *s++;

    if ( isupper ( s1 ) )
    {
      s1 = tolower ( s1 );
    }

    if ( s1 != *t++ )
    {
      return ( FALSE );
    }

  }
  *s0 = s;
  return ( TRUE );
}
/******************************************************************************/

int endcard ( char *s )

/******************************************************************************/
/*
  Purpose:

    ENDCARD checks to see whether the current record is an END statement.

  Modified:

    05 December 2007
*/
{
/*
  column 1 of card image
*/
  char *s0 = s;

  if ( *s==0 )
  {
    return ( TRUE );
  }
/*
  Search for "end" statement somewhere in the card image
*/
  while ( isspace ( *s ) )
  {
    ++s;
  }

  if ( *s != 'e' && *s != 'E' )
  {
    return ( FALSE );
  }

  s++;

  while ( isspace(*s) )
  {
    ++s;
  }

  if ( *s != 'n' && *s != 'N' )
  {
    return ( FALSE );
  }

  s++;
  while ( isspace(*s) )
  {
    ++s;
  }

  if ( *s != 'd' && *s != 'D' )
  {
    return(FALSE);
  }

  s++;
  while ( isspace(*s) )
  {
    ++s;
  }

/*
  Legitimate ending to "END" card?
  This must be modified to handle lines like "END FUNCTION".
*/
  if ( *s == '\0' || *s == '!' || *s == '\n' || (s - s0) >= 80 )
  {
    return(TRUE);
  }
  else
  {
    return(FALSE);
  }
}
/******************************************************************************/

void get_name ( char *s, char *f )

/******************************************************************************/
/*
  Purpose:

    GET_NAME creates a file name based on the name of the current module.

  Modified:

    05 December 2007
*/
{
  int i;
/*
  *S0 is the address of the first column of the card.
*/
  int count = 0;
  char *s0 = s;

  loop:

    if      ( compare ( &s, "function" ) )
    {
      goto bot;
    }
    else if ( compare ( &s, "module" ) )
    {
      goto bot;
    }
    else if ( compare ( &s, "procedure" ) )
    {
      goto bot;
    }
    else if ( compare ( &s, "program" ) )
    {
      goto bot;
    }
    else if ( compare ( &s, "subroutine" ) )
    {
      goto bot;
    }
    else if ( compare ( &s, "character" ) )
    {
      goto loop;
    }
    else if ( compare ( &s, "complex" ) )
    {
      goto loop;
    }
    else if ( compare ( &s, "double" ) )
    {
      goto loop;
    }
    else if ( compare ( &s, "integer" ) )
    {
      goto loop;
    }
    else if ( compare ( &s, "logical" ) )
    {
      goto loop;
    }
    else if ( compare ( &s, "precision" ) )
    {
      goto loop;
    }
    else if ( compare ( &s, "real" ) )
    {
      goto loop;
    }
    else if ( compare ( &s, "recursive" ) )
    {
      goto loop;
    }
/*
  Handle size complications like "complex *16" or "character *12".
*/
    else if ( compare ( &s, "*") )
    {
      for ( ++s ; isdigit ( *s ) || isspace ( *s )  ; ++s)
        ;
        goto loop;
      }
    else if ( compare ( &s, "blockdata" ) )
    {
      while ( isspace ( *s ) ) ++s;
/*
  No block data name given.  Use "BLOCKDATA".
*/
      if (*s == '\0')
      {
        sprintf ( f, "BLOCKDATA%d.%s", ++blkdatano, "f90" );
        return;
      }
      goto bot;
    }
    else
    {
      s = "";
    }

  bot:

  while ( isspace ( *s ) )
  {
    ++s;
  }
/*
  Extract the module name.
*/
  for ( i = 0; isalpha ( *s ) || isdigit ( *s ) || *s == '_' ; i++ )
  {
    if ( i >= NAMELENGTH || ( s - s0 ) >= 80 )
    {
      break;
    }
    f[i] = tolower ( *s++ );
    while ( isspace ( *s ) )
    {
      ++s;
    }
  }

/*
  Tack on the period and suffix to form the filename.
*/
  if ( i > 0)
  {
    f[i++] = '.';
    f[i++] = 'f';
    f[i++] = '9';
    f[i++] = '0';
    f[i++] = '\0';
  }
  else
  {
    count = count + 1;
    sprintf ( f, "main%d.%s", count, "f90" );
  }

  return;
}
/******************************************************************************/

void split_string ( FILE *file_input)
{
  char file_out_name[NAMELENGTH];
  char file_temp_name[NAMELENGTH];
  FILE *file_temp;
  int i;
  char in[BIG];
  int nline;
/*
  Set FILE_TEMP_NAME to a template name.
*/
  strcpy ( file_temp_name, "fsplit.XXXXX" );
/*
  MKSTEMP replaces the 'X' characters in FILE_TEMP_NAME by characters that
  result in a unique file name.
*/
  ( void ) mkstemp ( file_temp_name );
/*
  Open the temporary file for write access.
*/
  file_temp = fopen ( file_temp_name, "w" );

  if ( file_temp == NULL )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "FSPLIT90: Error!\n" );
    fprintf ( stderr, "  Can't open temporary file %s.\n", file_temp_name );
    exit ( EXIT_FAILURE );
  }
/*
  Read a line from the input unit.
*/
  nline = 0;

  while ( fgets ( in, BIG, file_input ) != NULL )
  {
    nline = nline + 1;
/*
  If the line is a comment line, output it, and get the next line.
  We're really hoping to see a module name.
*/
    if ( *in=='c' || *in=='C' || *in=='*' || *in=='!' )
    {
      fputs ( in, file_temp );
      continue;
    }

    for ( i = 0; i < 80; i++ )
    {
      if ( in[i] == '\0' || in[i] == '\n')
      {
        i = 80;
        break;
      }

      if (in[i] != ' ' && in[i] != '\t')
      {
        break;
      }
    }

    if ( i == 80 )
    {
      fputs ( in, file_temp );
      continue;
    }

    get_name ( in, file_out_name );

    if ( unlink(file_out_name),link(file_temp_name, file_out_name) == -1 ||
         unlink(file_temp_name) == -1)
    {
      fprintf ( stderr, "\n" );
      fprintf ( stderr, "FSPLIT90: Error!\n" );
      fprintf ( stderr, "  Cannot move %s to %s\n", file_temp_name, file_out_name );
      exit ( EXIT_FAILURE );
    }

    printf ( "%s\n", file_out_name );

    fputs ( in, file_temp );
/*
  Write all subsequent lines to this file until an END statement is encountered.
*/
    while ( !endcard(in) && fgets(in, BIG, file_input) )
    {
      fputs ( in, file_temp );
    }
/*
  Close the current file, and open the next temporary file.
*/
    ( void ) fclose ( file_temp );

    file_temp = fopen ( file_temp_name, "w" );

    if ( file_temp == NULL )
    {
      fprintf ( stderr, "\n" );
      fprintf ( stderr, "FSPLIT90: Error:\n" );
      fprintf ( stderr, "  Can't open temporary file %s.", file_temp_name );
      exit ( EXIT_FAILURE );
    }

  }

  if ( unlink ( file_temp_name ) == -1)
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "FSPLIT90: Error:\n" );
    fprintf ( stderr, "  Couldn't remove the temp file %s\n", file_temp_name );
    exit ( EXIT_FAILURE );
  }

  return;
}


void split_file ( FILE *file_input )

/******************************************************************************/
/*
  Purpose:

    SPLIT_FILE splits up the modules contained in a single file.

  Modified:

    05 December 2007
*/
{
  char file_out_name[NAMELENGTH];
  char file_temp_name[NAMELENGTH];
  FILE *file_temp;
  int i;
  char in[BIG];
  int nline;
/*
  Set FILE_TEMP_NAME to a template name.
*/
  strcpy ( file_temp_name, "fsplit.XXXXX" );
/*
  MKSTEMP replaces the 'X' characters in FILE_TEMP_NAME by characters that
  result in a unique file name.
*/
  ( void ) mkstemp ( file_temp_name );
/*
  Open the temporary file for write access.
*/
  file_temp = fopen ( file_temp_name, "w" );

  if ( file_temp == NULL )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "FSPLIT90: Error!\n" );
    fprintf ( stderr, "  Can't open temporary file %s.\n", file_temp_name );
    exit ( EXIT_FAILURE );
  }
/*
  Read a line from the input unit.
*/
  nline = 0;

  while ( fgets ( in, BIG, file_input ) != NULL )
  {
    nline = nline + 1;
/*
  If the line is a comment line, output it, and get the next line.
  We're really hoping to see a module name.
*/
    if ( *in=='c' || *in=='C' || *in=='*' || *in=='!' )
    {
      fputs ( in, file_temp );
      continue;
    }

    for ( i = 0; i < 80; i++ )
    {
      if ( in[i] == '\0' || in[i] == '\n')
      {
        i = 80;
        break;
      }

      if (in[i] != ' ' && in[i] != '\t')
      {
        break;
      }
    }

    if ( i == 80 )
    {
      fputs ( in, file_temp );
      continue;
    }

    get_name ( in, file_out_name );

    if ( unlink(file_out_name),link(file_temp_name, file_out_name) == -1 ||
         unlink(file_temp_name) == -1)
    {
      fprintf ( stderr, "\n" );
      fprintf ( stderr, "FSPLIT90: Error!\n" );
      fprintf ( stderr, "  Cannot move %s to %s\n", file_temp_name, file_out_name );
      exit ( EXIT_FAILURE );
    }

    printf ( "%s\n", file_out_name );

    fputs ( in, file_temp );
/*
  Write all subsequent lines to this file until an END statement is encountered.
*/
    while ( !endcard(in) && fgets(in, BIG, file_input) )
    {
      fputs ( in, file_temp );
    }
/*
  Close the current file, and open the next temporary file.
*/
    ( void ) fclose ( file_temp );

    file_temp = fopen ( file_temp_name, "w" );

    if ( file_temp == NULL )
    {
      fprintf ( stderr, "\n" );
      fprintf ( stderr, "FSPLIT90: Error:\n" );
      fprintf ( stderr, "  Can't open temporary file %s.", file_temp_name );
      exit ( EXIT_FAILURE );
    }

  }

  if ( unlink ( file_temp_name ) == -1)
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "FSPLIT90: Error:\n" );
    fprintf ( stderr, "  Couldn't remove the temp file %s\n", file_temp_name );
    exit ( EXIT_FAILURE );
  }

  return;
}
|]
