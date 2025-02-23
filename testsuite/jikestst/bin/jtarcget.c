/* $Id: jtarcget.c,v 1.4 1999/11/04 14:59:36 shields Exp $ */
/*
 This software is subject to the terms of the IBM Jikes Compiler
 License Agreement available at the following URL:
 http://www.ibm.com/research/jikes.
 Copyright (C) 1996, 1999, International Business Machines Corporation
 and others.  All Rights Reserved.
 You must accept the terms of that agreement to use this software.
*/

/* Unpack files packed by arcput, reading the packed file from
 * standard input and retrieving the member files.
 */
#include <stdlib.h>
#include <stdio.h>
 
FILE * ofile;
char fname[200];
char line[200];
main () {
 
    int i,j;
    ofile = (FILE *) 0;
    while(1) {
        if (fgets(line,200, stdin) == NULL) break;
        if (strlen(line)>3 && line[0] == '#'
                && line[1] == '#') {
            /* here if new file */
            if (ofile != (FILE *) 0) fclose(ofile);
            line[strlen(line)-1] = '\0'; /* discard end-of-line */
            for ((j=0,i=2); i<200; i++) { /* get file name */
                if (line[i] == '#' || line[i] == '\0') {
                    fname[j] = '\0';
                    break;
                }
                else {
                    fname[j++] = line[i];
                }
            }
            ofile = fopen(fname, "w");
            if (ofile == (FILE *)0 ) {
                printf("cannot open %s\n", fname);
                exit(1);
            }
            printf("copying %s\n", fname);
        }
        else {
            fputs(line,ofile);
        }
    }
    if (ofile != (FILE *) 0) fclose(ofile);
    exit(0);
}
