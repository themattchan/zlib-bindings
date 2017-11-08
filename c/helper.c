#include <zlib.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

z_stream * streaming_commons_create_z_stream(void)
{
	z_stream *ret = malloc(sizeof(z_stream));
	if (ret) {
		ret->zalloc = Z_NULL;
		ret->zfree = Z_NULL;
		ret->opaque = Z_NULL;
		ret->next_in = NULL;
		ret->avail_in = 0;
		ret->next_out = NULL;
		ret->avail_out = 0;
	}
	return ret;
}

int streaming_commons_inflate_init2(z_stream *stream, int window_bits)
{
	return inflateInit2(stream, window_bits);
}

int streaming_commons_deflate_init2(z_stream *stream, int level, int methodBits,
				    int memlevel, int strategy)
{
	return deflateInit2(stream, level, Z_DEFLATED, methodBits, memlevel, strategy);
}

int streaming_commons_inflate_set_dictionary(z_stream *stream, const unsigned char* dictionary,
					     unsigned int dictLength) {
        return inflateSetDictionary(stream, dictionary, dictLength);
}

int streaming_commons_deflate_set_dictionary(z_stream *stream, const unsigned char* dictionary,
					     unsigned int dictLength) {
        return deflateSetDictionary(stream, dictionary, dictLength);
}

void streaming_commons_free_z_stream_inflate (z_stream *stream)
{
	inflateEnd(stream);
	/* free(stream->next_in);
	 * free(stream->next_out);
	 * free(stream->msg); */
	free(stream);
}

void streaming_commons_set_avail_in (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_in = buff;
	stream->avail_in = avail;
}

void streaming_commons_set_avail_out (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_out = buff;
	stream->avail_out = avail;
}

int streaming_commons_call_inflate_noflush (z_stream *stream)
{
	return inflate(stream, Z_NO_FLUSH);
}

unsigned int streaming_commons_get_avail_in (z_stream *stream)
{
	return stream->avail_in;
}

unsigned int streaming_commons_get_avail_out (z_stream *stream)
{
	return stream->avail_out;
}

unsigned char* streaming_commons_get_next_in (z_stream *stream)
{
	return stream->next_in;
}

unsigned char* streaming_commons_get_next_out (z_stream *stream)
{
	return stream->next_out;
}

void streaming_commons_free_z_stream_deflate (z_stream *stream)
{
	deflateEnd(stream);
	free(stream);
}

int streaming_commons_call_deflate_noflush (z_stream *stream)
{
	return deflate(stream, Z_NO_FLUSH);
}

int streaming_commons_call_deflate_flush (z_stream *stream)
{
	return deflate(stream, Z_SYNC_FLUSH);
}

int streaming_commons_call_deflate_full_flush (z_stream *stream)
{
	return deflate(stream, Z_FULL_FLUSH);
}

int streaming_commons_call_deflate_finish (z_stream *stream)
{
	return deflate(stream, Z_FINISH);
}

z_stream * streaming_commons_copy_z_stream_inflate (z_stream *source)
{
	z_stream *dest = streaming_commons_create_z_stream();

	// no need to initialise, it happens in inflateCopy
	int ret = inflateCopy(dest, source);
	if (ret == Z_OK) {
//		fprintf(stderr, "COPY COPY COPY\n");
/* 		if (source->next_in != NULL) {
 * 			int inbytes = strlen((const char *) source->next_in);
 * //			fprintf(stderr, "HERE HERE HERE HERE HERE ");
 * 			dest->next_in = (unsigned char *)malloc(sizeof(char) * inbytes);
 * 			strncpy((char*)dest->next_in,(char*) source->next_in,inbytes);
 * //			fprintf(stderr, "COPY NEXT IN: %s\n", dest->next_in);
 * 		} */

		if (source->next_out != NULL) {
			if (strlen((const char*)source->next_out) == 0) {
// 				dest->next_out = NULL;
//				fprintf(stderr, "NO OUT BYTES\n");
//				fflush(stderr);
			} else {
				/* fprintf(stderr, "NEXT OUT LEN: %lu\n", strlen((const char*)source->next_out));
				 * fprintf(stderr, "HAS NEXT OUT: %s\n", (char*)source->next_out);
				 * fflush(stderr); */
				int outbytes = strlen( (const char *) source->next_out);
				dest->next_out = (unsigned char *)malloc(sizeof(char) * outbytes);
				strcpy((char*)dest->next_out, (char*)source->next_out);

				/* fprintf(stderr, "COPY NEXT OUT: %s\n", dest->next_out);
				 * fflush(stderr); */
			}
		}

/* 		if (source->msg != NULL) {
 * 			int msgbytes = strlen(source->msg);
 * 			dest->msg = (char *)malloc(sizeof(char) * msgbytes);
 * 			strcpy(dest->msg, source->msg);
 * //			fprintf(stderr, "COPY NEXT IN: %s\n", dest->next_in);
 * 		} */
//		fprintf(stderr, "C COPY DONEEEE\n");
//		fprintf(stderr, "COPY NEXT IN: %s\n", dest->next_in);/* FAIL  FAIL FAIL */
//		fprintf(stderr, "COPY NEXT OUT: %s\n", dest->next_out);
		return dest;
	} else {
//		streaming_commons_free_z_stream_inflate(dest);
		fprintf(stderr, "C COPY FAILLLLL\n");
		return NULL;
	}
}
