/**
 * @file debug.h
 * @brief Debugging functions 
 */
#ifndef _DEBUG_H_
#define _DEBUG_H_

/**
 * @brief prints a debug message to the screen if the level of the 
 *        message is higher than the current debug level
 * @param int level - level of debug message
 * @param char *fmt - format of message to write (a la printf)
 * @param ... - other args for format string
 */
void debug_printf(int level, const char *fmt, ...)
#ifdef __GNUC__
	__attribute__((format(printf, 2, 3)))
#endif
	;

#endif

