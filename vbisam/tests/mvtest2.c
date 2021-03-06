/*
 * Copyright:   (C) 2004 Mikhail Verkhovski
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<fcntl.h>
#include	<unistd.h>
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>
#include	<limits.h>
#include	<float.h>
#include	<time.h>

#include	<vbisam.h>

int	iVBRdCount = 0,
	iVBRdCommit = 0,
	iVBRdTotal = 0,
	iVBWrCount = 0,
	iVBWrCommit = 0,
	iVBWrTotal = 0,
	iVBDlCount = 0,
	iVBDlCommit = 0,
	iVBDlTotal = 0,
	iVBUpCount = 0,
	iVBUpCommit = 0,
	iVBUpTotal = 0;
int
main (int iArgc, char **ppcArgv)
{
	vb_rtd_t *vb_rtd =VB_GET_RTD;
	int	iResult,
		iLoop,
		iLoop2,
		iLoop3,
		iHandle;
	VB_UCHAR
		cRecord [256];
	struct	keydesc
		sKeydesc;
	char	cLogfileName [100],
		cCommand [100];
	char	cFileName [] = "IsamTest";

	memset (&sKeydesc, 0, sizeof (sKeydesc));
	sKeydesc.k_flags = COMPRESS;
	sKeydesc.k_nparts = 1;
	sKeydesc.k_start = 0;
	sKeydesc.k_leng = 2;
	sKeydesc.k_type = CHARTYPE;

	if (iArgc == 1) {
		printf ("Usage:\n\t%s create\nOR\n\t%s <#iterations>\n", ppcArgv [0], ppcArgv [0]);
		exit (1);
	}

	if (iArgc > 1 && strcmp (ppcArgv [1], "create") == 0) {
		iserase ((VB_CHAR*)cFileName);
		iHandle = isbuild ((VB_CHAR*)cFileName, 255, &sKeydesc, ISINOUT+ISFIXLEN+ISEXCLLOCK);
		if (iHandle < 0) {
			printf ("Error creating database: %d\n", vb_rtd->iserrno);
			exit (-1);
		}
		sKeydesc.k_flags |= ISDUPS;
		for (sKeydesc.k_start = 1; sKeydesc.k_start < 2; sKeydesc.k_start++) {
			if (isaddindex (iHandle, &sKeydesc)) {
				printf ("Error %d adding index %d\n", vb_rtd->iserrno, sKeydesc.k_start);
			}
		}
		isclose (iHandle);
		return (0);
	}
	srand (time (NULL));
	for (iLoop = 0; iLoop < atoi (ppcArgv [1]); iLoop++)
	{
		fflush(stdout);
		if (!(iLoop % 100)) {
			printf ("iLoop=%d\n", iLoop);
			fflush(stdout);
		}

		iVBDlCount = 0;
		iVBRdCount = 0;
		iVBUpCount = 0;
		iVBWrCount = 0;
		iHandle = isopen ((VB_CHAR*)cFileName, ISINOUT+ISFIXLEN+ISAUTOLOCK);
		if (iHandle < 0) {
			printf ("Error opening database: %d\n", vb_rtd->iserrno);
			exit (-1);
		}

		for (iLoop2 = 0; iLoop2 < 100; iLoop2++)
		{
			for (iLoop3 = 0; iLoop3 < 256; iLoop3++) {
				cRecord [iLoop3] = rand () % 256;
			}

			switch (rand () % 4) {
			case	0:
				if ((iResult = iswrite (iHandle, (VB_CHAR *) cRecord)) != 0) {
					if (vb_rtd->iserrno != EDUPL && vb_rtd->iserrno != ELOCKED) {
						printf ("Error writing: %d\n", vb_rtd->iserrno);
						goto err;
					}
				} else {
					iVBWrCount++;
				}
				break;

			case	1:
				if ((iResult = isread (iHandle, (VB_CHAR *)cRecord, ISEQUAL)) != 0) {
					if (vb_rtd->iserrno == ELOCKED) {
						;  printf ("Locked during deletion\n"); 
					} else if (vb_rtd->iserrno != ENOREC) {
						printf ("Error reading: %d\n", vb_rtd->iserrno);
						goto err;
					}
				} else {
					iVBRdCount++;
				}
				break;

			case	2:
				for (iLoop3 = 0; iLoop3 < 256; iLoop3++) {
					cRecord [iLoop3] = rand () % 256;
				}
				if ((iResult = isrewrite (iHandle, (VB_CHAR *)cRecord)) != 0) {
					if (vb_rtd->iserrno == ELOCKED) {
						;  printf ("Locked during rewrite\n"); 
					} else if (vb_rtd->iserrno != ENOREC) {
						printf ("Error rewriting: %d\n", vb_rtd->iserrno);
						goto err;
					}
				} else {
					iVBUpCount++;
				}
				break;

			case	3:
				if ((iResult = isdelete (iHandle, (VB_CHAR *)cRecord)) != 0) {
					if (vb_rtd->iserrno == ELOCKED) {
						; printf ("Locked during deletion\n"); 
					} else if (vb_rtd->iserrno != ENOREC) {
						printf ("Error deleting: %d\n", vb_rtd->iserrno);
						goto err;
					}
				}
				else
					iVBDlCount++;
				break;
			}
		}

		iResult = isflush (iHandle);
		if (iResult < 0) {
			printf ("Error flush: %d\n", vb_rtd->iserrno);
			exit (-1);
		}
		iResult = isclose (iHandle);
		if (iResult < 0) {
			printf ("Error closing database: %d\n", vb_rtd->iserrno);
			exit (-1);
		}

		iVBDlTotal += iVBDlCount;
		iVBRdTotal += iVBRdCount;
		iVBUpTotal += iVBUpCount;
		iVBWrTotal += iVBWrCount;
		/*
		switch (rand () % 2) {
		case	0:
			iVBDlCommit += iVBDlCount;
			iVBRdCommit += iVBRdCount;
			iVBUpCommit += iVBUpCount;
			iVBWrCommit += iVBWrCount;
			iResult = iscommit ();
			if (iResult < 0) {
				printf ("Error commit: %d\n", vb_rtd->iserrno);
				exit (-1);
			}
			break;

		case	1:
			iResult = isrollback ();
			if (iResult < 0) {
				if (vb_rtd->iserrno == EDUPL || vb_rtd->iserrno == ENOREC) {
					printf ("Same BUG (%d) as in C-ISAM!\n", vb_rtd->iserrno);
				} else {
					printf ("Error rollback: %d\n", vb_rtd->iserrno);
					exit (-1);
				}
			}
			break;
		}
		*/
	}
err:
	printf ("                 Total \n");
	printf ("              -------- --------\n");
	printf ("Delete Count: %8d \n", iVBDlTotal);
	printf ("Read   Count: %8d \n", iVBRdTotal);
	printf ("Update Count: %8d \n", iVBUpTotal);
	printf ("Write  Count: %8d \n", iVBWrTotal);
	printf ("              -------- --------\n");
	printf ("OPS OVERALL : %8d \n", (iVBDlTotal + iVBRdTotal + iVBUpTotal + iVBWrTotal));
	printf ("                       ========\n");
	return (iResult);
}
