/**
 * @file plugin.h
 * @brief interfaces for handling custom debconf widget implementations
 */
#ifndef _PLUGIN_H_
#define _PLUGIN_H_

struct configuration;
struct frontend;
struct question;

struct plugin {
    /** type name */
    char *name;
    /** library module handle */
    void *module;
    /** handler function */
    void *handler;
};

struct plugin *plugin_new(const char *frontend, const char *filename);
void plugin_delete(struct plugin *plugin);
struct plugin *plugin_find(struct frontend *frontend, const char *name);
struct plugin *plugin_iterate(struct frontend *frontend, void **state);

#endif
