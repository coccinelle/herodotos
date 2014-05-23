// File ver 2

#include <stdio.h>

int main() {
	int err = 0;
	int version = 2;

	error(err);
	printf("This is version "
	             "%d", version);

	fixed();
	error(version);
	return err;
}
