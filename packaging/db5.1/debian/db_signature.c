#include <stdio.h>
#include "db.h"

extern u_int32_t __env_struct_sig();

int main(int argc, char **argv) {

	printf("%x\n", __env_struct_sig());

	return 0;
}
