/**
 *
 * @file confmodule.h
 * @brief Configuration module object definition
 *
 */
#ifndef _CONFMODULE_H_
#define _CONFMODULE_H_

#include "common.h"
#include <signal.h>

/*
 *  For full backup sort, questions are not flagged as being seen
 *  as soon as they are displayed, but only when session is over.
 *  The list of displayed questions is managed by a stack, which
 *  is the seen_questions member of the confmodule structure.
 */
enum seen_action {
        STACK_SEEN_ADD,       /*  Add a question to the stack       */
        STACK_SEEN_REMOVE,    /*  Remove a question from the stack  */
        STACK_SEEN_SAVE       /*  Questions are flagged as seen and
                                  removed from the etack */
};

/* Set this in a signal handler to cause confmodule_communicate() to return
 * at the next opportunity.
 */
extern volatile sig_atomic_t signal_received;

/* If SIGCHLD is handled, put the status here. */
extern volatile sig_atomic_t sigchld_status;

struct configuration;
struct template_db;
struct question_db;
struct frontend;

struct confmodule {
	struct configuration *config;
	struct template_db *templates;
	struct question_db *questions;
	struct frontend *frontend;
	pid_t pid;
	int infd, outfd;
	int exitcode;
	int backed_up;
	const char *owner;
	char **seen_questions;
	int number_seen_questions;

	/* methods */
    /*
     * @brief runs a config script, connected to the confmodule
     * @param struct confmodule *mod - confmodule object
     * @param int argc - number of arguments to pass to config script
     * @param char **argv - argument array
     * @return pid_t - pid of config script, -1 if error
     */
	pid_t (*run)(struct confmodule *, int argc, char **argv);

    /**
     * @brief handles communication between a config script and the 
     * confmodule
     * @param struct confmodule *mod - confmodule object
     * @return int - DC_OK, DC_NOTOK
     */
	int (*communicate)(struct confmodule *mod);

    /**
     * @brief process one command
     * @param const char *cmd - command to execute
     * @param char *out - return buffer
     * @param size_t outsize - size of return buffer
     * @return char* - DC_OK, DC_NOTOK + error message
     */
    char* (*process_command)(struct confmodule *mod, char *cmd);

    /**
     * @brief Shuts down the confmodule
     * @param struct confmodule *mod - confmodule object
     * @return int - exit code of the config script
     */
	int (*shutdown)(struct confmodule *mod);

    /**
     * @brief Stack for already seen questions, to help backing up
     * @param struct confmodule *mod - confmodule object
     * @param int action - push, pop or sync values
     * @return int - DC_OK, DC_NOTOK
     */
	int (*update_seen_questions)(struct confmodule *mod, enum seen_action action);

    /**
     * @param struct confmodule *mod - confmodule object
     * @return int - DC_OK, DC_NOTOK
     */
	int (*save)(struct confmodule *mod);
};

/**
 * @brief creates a new confmodule object
 * @param struct configuration *config - configuration parameters
 * @param struct database *db - database object
 * @param struct frontend *frontend - frontend UI object
 * @return struct confmodule * - newly created confmodule
 */
struct confmodule *confmodule_new(struct configuration *,
	struct template_db *, struct question_db *, struct frontend *);

/*
 * @brief destroys a confmodule object
 * @param confmodule *mod - confmodule object to destroy
 */
void confmodule_delete(struct confmodule *mod);

#endif
