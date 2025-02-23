/* $Id: jtarcput.c,v 1.4 1999/11/04 14:59:36 shields Exp $ */
/*
 This software is subject to the terms of the IBM Jikes Compiler
 License Agreement available at the following URL:
 http://www.ibm.com/research/jikes.
 Copyright (C) 1996, 1999, International Business Machines Corporation
 and others.  All Rights Reserved.
 You must accept the terms of that agreement to use this software.
*/


/* pack several files into one by reading a list of file names
 * from standard input. The first name is that of the output file,
 * remaining names are those of input files to be copied to the
 * output file
 * with a prefix line before each file of the form
 * ##filename##
 */
#include <stdlib.h>
#include <stdio.h>
FILE * ifile;
FILE * ofile;
char line[200];
char iname[50];
char oname[50];
int lines=0;
int this=0;
main () {
 
    if (fgets(oname, 50, stdin) == NULL ) {
        printf("cannot get output file name\n");
        exit(1);
    }
    oname[strlen(oname)-1] = '\0'; /* discard end-of-line */
    ofile = fopen(oname, "w");
    if (ofile == (FILE *)0) {
        printf("cannot open output file %s\n",oname);
        exit(1);
    }
    printf("packing to %s\n",oname);
    while(1) {
        if (fgets(iname, 50, stdin) == NULL) {
	  if (this) { /* print line count of current file */
	    printf("\t%6d\n",this);
	  }
	  break;
	}
        iname[strlen(iname)-1] = '\0'; /* discard end-of-line */
        ifile = fopen(iname, "r");
        if (ifile == (FILE *)0 ) {
            printf("cannot open %s\n", iname);
            exit(1);
        }
	if (this) { /* print line count of prior file */
	  printf("\t%6d\n",this);
	}
        printf("%6d %s",++lines,iname);
        fprintf(ofile,"##%s##\n",iname);
	this=0;
 
        /* copy the file */
        while (fgets(line, 200, ifile) != NULL) {
                fputs(line, ofile);
		lines++;
		this++;
        }
        fclose(ifile);
    }
    exit(0);
}
