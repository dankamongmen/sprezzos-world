#include "configuration.h"

#define FILENAME "test.conf"

int main(int argc, char **argv)
{
	struct configuration *cfg = config_new();
	cfg->read(cfg, FILENAME);
	cfg->dump(cfg);
	config_delete(cfg);
	return 0;
}
