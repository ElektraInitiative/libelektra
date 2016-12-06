/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */


#include <kdbassert.h>
#include <kdblogger.h>
#include <kdbopmphm.h>

#include <stdio.h>  //debug
#include <string.h> //strlen
#include <time.h>   //time

double opmphmRatio = 0.75;

/* Prints the bipartite graph to a dot file, to draw it, for debug purpose. */
void opmphmPrintGraph (Edge * edges, void ** data, opmphmGetString fpOpmhpmGetString, size_t n)
{
	FILE * f = fopen ("opmphmGraph.dot", "wr");
	fprintf (f, "graph graphname {\n");
	for (size_t i = 0; i < n; ++i)
	{
		uint32_t h1 = edges[i].h[1];
		uint32_t h2 = edges[i].h[2];
		fprintf (f, "	%u -- %u [label=\"%s\"]\n", h1, h2, fpOpmhpmGetString (data[i]));
	}
	fprintf (f, "}\n");
	fclose (f);
}

/** The first phase of the order preserving minimal perfect hash map build. */
int opmphmMapping (OPMPHM * opmphm, Vertex * vertices, Edge * edges, OPMPHMinit * init, size_t n)
{
	// set the seeds, for the hash function
	for (int i = 0; i < 3; ++i)
	{
		opmphm->opmphmHashFunctionSeeds[i] = opmphmRandom (&(init->seed));
	}
	size_t r = opmphmRatio * n;
	ELEKTRA_LOG ("OPMPHM: Mapping: r=%lu seed[0]=%u seed[1]=%u seed[2]=%u\n", r, opmphm->opmphmHashFunctionSeeds[0],
		     opmphm->opmphmHashFunctionSeeds[1], opmphm->opmphmHashFunctionSeeds[2]);
	// init used values
	for (size_t i = 0; i < r * 2; ++i)
	{
		vertices[i].degree = 0;
		vertices[i].firstEdge = -1;
	}
	ELEKTRA_LOG_DEBUG ("OPMPHM: Mapping: Keys:\n");
	for (size_t i = 0; i < n; ++i)
	{
		// set the resulting hash values for each key
		const char * name = init->getString (init->data[i]);
		ELEKTRA_LOG_DEBUG ("%s\n", name);
#ifndef OPMPHM_TEST
		edges[i].h[0] = opmphmHashfunction ((const uint32_t *)name, strlen (name), opmphm->opmphmHashFunctionSeeds[0]) % n;
		edges[i].h[1] = opmphmHashfunction ((const uint32_t *)name, strlen (name), opmphm->opmphmHashFunctionSeeds[1]) % r;
		edges[i].h[2] = (opmphmHashfunction ((const uint32_t *)name, strlen (name), opmphm->opmphmHashFunctionSeeds[2]) % r) + r;
#endif
		// add each key to both lists
		edges[i].nextEdge[0] = vertices[edges[i].h[1]].firstEdge;
		vertices[edges[i].h[1]].firstEdge = i;
		++vertices[edges[i].h[1]].degree;
		edges[i].nextEdge[1] = vertices[edges[i].h[2]].firstEdge;
		vertices[edges[i].h[2]].firstEdge = i;
		++vertices[edges[i].h[2]].degree;
	}
	// verify that all triples are disjunct, if not this function will be called again
	for (size_t i = 0; i < r; ++i)
	{
		for (int e0 = vertices[i].firstEdge; e0 != -1; e0 = edges[e0].nextEdge[0])
		{
			for (int e1 = edges[e0].nextEdge[0]; e1 != -1; e1 = edges[e1].nextEdge[0])
			{
				if (edges[e0].h[0] == edges[e1].h[0] && edges[e0].h[2] == edges[e1].h[2])
				{
					return 1;
				}
			}
		}
	}
	return 0;
}


/**
 * Hash function
 * By Bob Jenkins, May 2006
 * http://burtleburtle.net/bob/c/lookup3.c
 * Original name: hashlitte (the little endian part)
 * For now assuming little endian maschine
 */
uint32_t opmphmHashfunction (const void * key, size_t length, uint32_t initval)
{
	uint32_t a, b, c;
	a = b = c = 0xdeadbeef + ((uint32_t)length) + initval;
	const uint32_t * k = (const uint32_t *)key;
	while (length > 12)
	{
		a += k[0];
		b += k[1];
		c += k[2];
		OPMPHM_HASHFUNCTION_MIX (a, b, c)
		length -= 12;
		k += 3;
	}
	switch (length)
	{
	case 12:
		c += k[2];
		b += k[1];
		a += k[0];
		break;
	case 11:
		c += k[2] & 0xffffff;
		b += k[1];
		a += k[0];
		break;
	case 10:
		c += k[2] & 0xffff;
		b += k[1];
		a += k[0];
		break;
	case 9:
		c += k[2] & 0xff;
		b += k[1];
		a += k[0];
		break;
	case 8:
		b += k[1];
		a += k[0];
		break;
	case 7:
		b += k[1] & 0xffffff;
		a += k[0];
		break;
	case 6:
		b += k[1] & 0xffff;
		a += k[0];
		break;
	case 5:
		b += k[1] & 0xff;
		a += k[0];
		break;
	case 4:
		a += k[0];
		break;
	case 3:
		a += k[0] & 0xffffff;
		break;
	case 2:
		a += k[0] & 0xffff;
		break;
	case 1:
		a += k[0] & 0xff;
		break;
	case 0:
		return c;
	}
	OPMPHM_HASHFUNCTION_FINAL (a, b, c);
	return c;
}

/*
 * This Random function comes from:
 * S. Park & K. Miller
 * Random Number Generator: Good ones are Hard to find
 * http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf
 * 1988
 */
uint32_t opmphmRandom (unsigned int * seedp)
{
	ELEKTRA_ASSERT (seedp != NULL, "NULL pointer passed");
	*seedp = (16807 * *seedp) % 2147483647;
	return rand_r (seedp);
}
