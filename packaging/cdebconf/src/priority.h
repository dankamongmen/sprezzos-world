/**
 * @file priority.c
 * @brief routines to handle priority field parsing and comparison
 */
#ifndef _PRIORITY_H_
#define _PRIORITY_H_

/**
 * @brief compares two priorities
 * @param const char *p1, const char *p2 - priorities to compare
 * @return int - <0 if p1<p2, ==0 if p1==p2, >0 if p1>p2
 */
int priority_compare(const char *p1, const char *p2);

#endif
