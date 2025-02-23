/* $Id: jtarccat.c,v 1.4 1999/11/04 14:59:35 shields Exp $ */
/*
 This software is subject to the terms of the IBM Jikes Compiler
 License Agreement available at the following URL:
 http://www.ibm.com/research/jikes.
 Copyright (C) 1996, 1999, International Business Machines Corporation
 and others.  All Rights Reserved.
 You must accept the terms of that agreement to use this software.
*/

/* pack several files into one by reading a list of file names
 * from standard input, copy them to standard output,
 * with a prefix line before each file of the form
 * ##filename##
 */
#include <stdlib.h>
#include <stdio.h>
FILE * ifile;
FILE * ofile;
char line[500];
char iname[100];
main () {
 
    ofile = stdout;
    while(1) {
        if (fgets(iname, 100, stdin) == NULL) {
	  break;
	}
        iname[strlen(iname)-1] = '\0'; /* discard end-of-line */
        ifile = fopen(iname, "r");
        if (ifile == (FILE *)0 ) {
            printf("cannot open %s\n", iname);
            exit(1);
        }
        fprintf(ofile,"##%s##\n",iname);
 
        /* copy the file */
        while (fgets(line, 500, ifile) != NULL) {
                fputs(line, ofile);
        }
        fclose(ifile);
    }
    exit(0);
}
