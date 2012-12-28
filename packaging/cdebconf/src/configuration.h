/**
 *
 * @file configuration.h
 * @brief Configuration management routines
 *
 */
#ifndef _CONFIGURATION_H_
#define _CONFIGURATION_H_

/* This is roughly based on the APT configuration class */

struct configitem {
	char *tag;
	char *value;
	struct configitem *parent, *child, *next;
};

/**
 * @brief Configuration management class
 */
struct configuration {
    /**
     * @brief configuration data
     */
	struct configitem *root;

    /**
     * @brief Get a configuration item with a given tag (string)
     * @param cfg configuration object
     * @param tag Tag of configuration item
     * @param defaultvalue Default value
     * @return Value of configuration item
     */
	const char *(*get)(struct configuration *cfg, const char *tag, 
		const char *defaultvalue);

    /**
     * @brief Get a configuration item with a given tag (integer)
     * @param cfg configuration object
     * @param tag Tag of configuration item
     * @param defaultvalue Default value
     * @return Value of configuration item
     */
	int (*geti)(struct configuration *cfg, const char *tag, 
		int defaultvalue);

    /**
     * @brief Set the value of a configuration item (string)
     * @param cfg Configuration object
     * @param tag Tag of configuration item
     * @param value New value
     */
	void (*set)(struct configuration *cfg, const char *tag,
		const char *value);

    /**
     * @brief Set the value of a configuration item (integer)
     * @param cfg Configuration object
     * @param tag Tag of configuration item
     * @param value New value
     */
	void (*seti)(struct configuration *cfg, const char *tag,
		int value);

    /**
     * @brief Checks to see if a given configuration item exists
     * @param cfg Configuration object
     * @param tag Tag of configuration item
     * @return 1 if item exists
     */
	int (*exists)(struct configuration *cfg, const char *tag);

    /**
     * @brief Populate configuration object with data from file
     * @param cfg Configuration object
     * @param filename Configuration file to read
     * @return 1 on success, 0 on error
     */
	int (*read)(struct configuration *cfg, const char *filename);
    
    /**
     * @brief Dump the contents of a configuration object to stdout
     * @param cfg Configuration object
     */
	void (*dump)(struct configuration *cfg);

    /**
     * @brief returns an internal pointer to a tree structure 
     *        representing a node with the given tag
     * @param cfg Configuration object
     * @param tag Tag of configuration item
     * @warning external callers should not change the structure of 
     *          the tree returned
     */
	struct configitem *(*tree)(struct configuration *cfg, 
        const char *tag);
};

/**
 * @brief Creates a configuration object
 */
struct configuration *config_new(void);

/**
 * @brief Deletes a configuration object
 */
void config_delete(struct configuration *);

#endif
