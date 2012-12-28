/**
 *
 * @file commands.h
 * @brief Implementation of each command in the spec
 *
 */
#ifndef _COMMANDS_H_
#define _COMMANDS_H_

#include "confmodule.h"

#define CMDSTATUS_SUCCESS           0
#define CMDSTATUS_ESCAPEDDATA       1
#define CMDSTATUS_BADQUESTION       10
#define CMDSTATUS_BADPARAM          15
#define CMDSTATUS_SYNTAXERROR       20
#define CMDSTATUS_INPUTINVISIBLE    30
#define CMDSTATUS_BADVERSION        30
#define CMDSTATUS_GOBACK            30
#define CMDSTATUS_PROGRESSCANCELLED 30
#define CMDSTATUS_INTERNALERROR     100

/**
 * @brief Handler function type for each command
 * @param struct confmodule *mod - confmodule object
 * @param int argc - number of arguments
 * @param char **argv - argument array
 * @param char *out - output buffer
 * @param size_t outsize - output buffer size
 * @return int - DC_NOTOK if error, DC_OK otherwise
 */
typedef char *(*command_function_t)(struct confmodule *mod, char *arg);


/**
 * @brief mapping of command name to handler function
 */
typedef struct {
    const char *command;
    command_function_t handler;
} commands_t;

/**
 * @brief Handler for the INPUT debconf command
 *
 * Adds a question to the list of questions to be asked if appropriate.
 */
char *command_input(struct confmodule *, char *);

/**
 * @brief Handler for the CLEAR debconf command
 *
 * Removes any questions currently in the queue
 */
char *command_clear(struct confmodule *, char *);

/**
 * @brief handler for the VERSION debconf command
 *
 * Checks to see if the version required by a confmodule script
 * is compatible with the debconf version we recognize
 */
char *command_version(struct confmodule *, char *);

/**
 * @brief handler for the CAPB debconf command
 * 
 * Exchanges capability information between the confmodule and
 * the frontend
 */
char *command_capb(struct confmodule *, char *);

/**
 * @brief handler for the TITLE debconf command
 *
 * Sets the title in the frontend
 */
char *command_title(struct confmodule *, char *);

/**
 * @brief handler for the BEGINBLOCK debconf command
 * @warning Not yet implemented
 */
char *command_beginblock(struct confmodule *, char *);

/**
 * @brief handler for the ENDBLOCK debconf command
 * @warning Not yet implemented
 */
char *command_endblock(struct confmodule *, char *);

/**
 * @brief handler for the GO debconf command
 *
 * Asks all pending questions and save the answers into the debconf
 * database.
 *
 * Frontend should return CMDSTATUS_GOBACK only if the confmodule
 * supports backing up
 */
char *command_go(struct confmodule *, char *);

/**
 * @brief handler for the GET debconf command
 *
 * Retrieves the value of a given template
 */
char *command_get(struct confmodule *, char *);

/**
 * @brief handler for the SET debconf command
 *
 * Sets the value of a given template
 */
char *command_set(struct confmodule *, char *);

/**
 * @brief handler for the RESET debconf command
 *
 * Resets the value of a given template to the default
 */
char *command_reset(struct confmodule *, char *);

/**
 * @brief handler for the SUBST debconf command
 *
 * Registers a substitution variable/value for a template
 */
char *command_subst(struct confmodule *, char *);

/**
 * @brief handler for the REGISTER debconf command
 */
char *command_register(struct confmodule *, char *);

/**
 * @brief handler for the UNREGISTER debconf command
 */
char *command_unregister(struct confmodule *, char *);

/**
 * @brief handler for the PURGE debconf command
 *
 * Removes all questions owned by a given owner
 */
char *command_purge(struct confmodule *, char *);

/**
 * @brief handler for the METAGET debconf command
 *
 * Retrieves a given attribute for a template
 */
char *command_metaget(struct confmodule *, char *);

/**
 * @brief handler for the FGET debconf command
 * 
 * Retrieves a given flag value for a template
 */
char *command_fget(struct confmodule *, char *);

/**
 * @brief handler for the FSET debconf command
 * 
 * Sets a given flag value for a template
 */
char *command_fset(struct confmodule *, char *);

/**
 * @brief handler for the EXISTS debconf command
 *
 * Checks to see if a template exists
 */
char *command_exist(struct confmodule *, char *);

/**
 * @brief handler for the STOP debconf command
 *
 * Finishes the debconf session
 */
char *command_stop(struct confmodule *, char *);

/**
 * @brief handler for the PROGRESS debconf command
 *
 * Progress bar handling
 *
 * @warning This is not yet in the debconf spec
 */
char *command_progress(struct confmodule *, char *);

/**
 * @brief handler for the X_LOADTEMPLATEFILE debconf command
 *
 * Loads a new template into the debconf database
 *
 * @warning This is not in the debconf spec
 */
char *command_x_loadtemplatefile(struct confmodule *, char *);

/**
 * @brief handler for the X_SAVE debconf command
 *
 * Saves the debconf database to disk
 *
 * @warning This is not in the debconf spec
 */
char *command_x_save(struct confmodule *, char *);

/**
 * @brief handler for the SETTITLE debconf command
 *
 * Set the debconf title to the discription of the template specified
 *
 * @warning This is not in the debconf spec
 */
char *command_settitle(struct confmodule *mod, char *arg);

/**
 * @brief handler for the INFO debconf command
 *
 * Displays the given template as an out-of-band informative message. Unlike
 * inputting a note, this doesn't require an acknowledgement from the user,
 * and depending on the frontend it may not even be displayed at all.
 *
 * @warning This is not in the debconf spec
 */
char *command_info(struct confmodule *mod, char *);

/**
 * @brief handler for the DATA debconf command
 *
 * Read template data sent by the client
 *
 * @warning This is not in the debconf spec
 */
char *command_data(struct confmodule *mod, char *);

#endif
