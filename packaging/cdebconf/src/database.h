/**
 *
 * @file database.h
 * @brief Database class definition
 *
 */
#ifndef _DATABASE_H_
#define _DATABASE_H_

#include "constants.h"

/* Debconf database interfaces */

struct configuration;
struct template;
struct template_db;
struct question;
struct question_db;

#define DC_LOADTEMPLATE_NONE  (0)
#define DC_LOADTEMPLATE_MERGE (1<<0)

/**
 * @brief Methods for a template database module
 */
struct template_db_module {
    int (*initialize)(struct template_db *db, struct configuration *cfg);
    int (*shutdown)(struct template_db *db);
    int (*load)(struct template_db *db);
    int (*reload)(struct template_db *db);
    int (*save)(struct template_db *db);
    int (*set)(struct template_db *db, struct template *t);
    struct template *(*get)(struct template_db *db, const char *name);
    int (*remove)(struct template_db *, const char *name);
    int (*lock)(struct template_db *, const char *name);
    int (*unlock)(struct template_db *, const char *name);
    struct template *(*iterate)(struct template_db *db, void **iter);
    int (*accept)(struct template_db *, const char *name, const char *type);
};

/**
 * @brief Methods for a question database module
 */
struct question_db_module {
    int (*initialize)(struct question_db *db, struct configuration *cfg);
    int (*shutdown)(struct question_db *db);
    int (*load)(struct question_db *db);
    int (*save)(struct question_db *db);
    int (*set)(struct question_db *, struct question *q);
    struct question *(*get)(struct question_db *db, const char *name);
    int (*disown)(struct question_db *, const char *name, const char *owner);
    int (*disownall)(struct question_db *, const char *owner);
    int (*remove)(struct question_db *, const char *name);
    int (*lock)(struct question_db *, const char *name);
    int (*unlock)(struct question_db *, const char *name);
    int (*is_visible)(struct question_db *, const char *name, const char *priority);
    struct question *(*iterate)(struct question_db *, void **iter);
    int (*accept)(struct question_db *, const char *name, const char *type);
};

/**
 * @brief Template database object
 */
struct template_db {
    /** db module name */
    char *modname;
    /** db module handle */
    void *handle;
    /** configuration data */
    struct configuration *config;
    /** config path - base of instance configuration */
    char configpath[DEBCONF_MAX_CONFIGPATH_LEN];
    /** private data */
    void *data; 

    /** methods */
    struct template_db_module methods;
};

/**
 * @brief Question database object
 */
struct question_db {
    /** db module name */
    char *modname;
    /** db module handle */
    void *handle;
    /** configuration data */
    struct configuration *config;
    /** config path - base of instance configuration */
    char configpath[DEBCONF_MAX_CONFIGPATH_LEN];
    /** private data */
    void *data; 
    /** template database */
    struct template_db *tdb;

    /** methods */
    struct question_db_module methods;
};

/**
 * @brief Create a new template db object
 * @param cfg configuration
 * @param instance Name of instance (NULL for default)
 * @return Newly created db object
 */
struct template_db *template_db_new(struct configuration *cfg, const char *instance);

/**
 * @brief Destroy a template db oject
 * @param db Object to destroy
 */
void template_db_delete(struct template_db *db);

/**
 * @brief Loads all the templates from a file
 * @param tdb template database object
 * @param qdb question database object
 * @param filename file to load templates from
 * @param owner owner of the templates
 */
void template_db_loadfile(struct template_db *tdb, struct question_db *qdb, const char *filename, const char *owner, int flagsb);

/**
 * @brief Create a new question db object
 * @param cfg configuration
 * @param tdb associated template database object
 * @param instance Name of instance (NULL for default)
 * @return Newly created db object
 */
struct question_db *question_db_new(struct configuration *cfg, struct template_db *tdb, const char *instance);

/**
 * @brief Destroy a question db oject
 * @param db Object to destroy
 */
void question_db_delete(struct question_db *db);

#endif
