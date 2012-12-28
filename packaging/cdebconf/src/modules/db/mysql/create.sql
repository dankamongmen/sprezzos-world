-- Database creation script for cdebconf MySQL backend
-- Note: This depends on a fairly recent version of mysql

-- WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! 
-- This will delete your database and start things from scratch!
-- WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! 
DROP DATABASE IF EXISTS DebianConfig;
CREATE DATABASE IF NOT EXISTS DebianConfig;

\u DebianConfig

CREATE TABLE Hosts (
	id		INT UNSIGNED NOT NULL AUTO_INCREMENT,
	name		CHAR(50),
	PRIMARY KEY(id)
);

CREATE TABLE Packages (
	id		INT UNSIGNED NOT NULL AUTO_INCREMENT,
	name		CHAR(80),
	version		CHAR(80),
	PRIMARY KEY(id),
	KEY idx_Packages_name_version (name, version)
);

CREATE TABLE Templates (
	id		INT UNSIGNED NOT NULL AUTO_INCREMENT,
	package_id	INT UNSIGNED,
	tag		CHAR(80),
	ttype		INT UNSIGNED,
	default_value	CHAR(255),
	choices		CHAR(255),
	description	CHAR(80),
	ext_description	CHAR(255),
	help_id		INT UNSIGNED,
	modified	TIMESTAMP,
	PRIMARY KEY(id),
	KEY idx_Templates_tag(tag)
);

CREATE TABLE Questions (
	id		INT UNSIGNED NOT NULL AUTO_INCREMENT,
	template_id	INT UNSIGNED,
	host_id		INT UNSIGNED,
	config_id	INT UNSIGNED,
	tag		INT UNSIGNED,
	value		CHAR(255),
	flags		INT UNSIGNED,
	modified	TIMESTAMP,
	PRIMARY KEY(id),
	KEY idx_Questions_tag(tag)
);

CREATE TABLE QuestionOwners (
	question_id	INT UNSIGNED NOT NULL ,
	owner		CHAR(255),
	KEY idx_QuestionOwners_id (question_id)
);

CREATE TABLE QuestionVariables (
	question_id	INT UNSIGNED NOT NULL,
	variable	CHAR(255),
	value		CHAR(255),
	KEY idx_QuestionVariables_id (question_id)
);

-- EOF
