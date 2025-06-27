#include <stdio.h>
#include <unistd.h>
#include <stdbool.h>
#include <getopt.h>
#include <stdlib.h>

#include "common.h"
#include "file.h"
#include "parse.h"
#include "srvpoll.h"

void print_usage(char *argv[]) {
    printf("Usage: %s -n -f <database_file>\n", argv[0]);
    printf("Options:\n");
    printf("\t -n          create new database file\n");
    printf("\t -f <file>   (required) database file path\n");
    printf("\t -p <port>   (required) port to listen on\n");
    exit(1);
}

int main(int argc, char *argv[]) { 
	char *filepath = NULL;
	char *portarg = NULL;
	unsigned short port = 0;
	bool newfile = false;
	int c;

	int dbfd = -1;
	struct dbheader_t *dbhdr = NULL;
	struct employee_t *employees = NULL;

	while ((c = getopt(argc, argv, "nf:p:")) != -1) {
		switch (c) {
			case 'n':
				newfile = true;
				break;
			case 'f':
				filepath = optarg;
				break;
			case 'p':
				portarg = optarg;
				port = atoi(portarg);
				break;
			case '?':
				fprintf(stderr, "Unknown option -%c\n", c);
				print_usage(argv);
				return -1;
			default:
				return -1;

		}
	}

    if (filepath == NULL) {
		fprintf(stderr, "Filepath is a required argument\n");
		print_usage(argv);

		return 0;
	}

	if (port == 0) {
		fprintf(stderr, "Bad port: %s\n", portarg);
        print_usage(argv);
    }

    if (newfile) {
		dbfd = create_db_file(filepath);
		if (dbfd == STATUS_ERROR) {
			fprintf(stderr, "Error creating database file: %s\n", filepath);
			return -1;
		}

		if (create_db_header(dbfd, &dbhdr) == STATUS_ERROR) {
			fprintf(stderr, "Error creating database header in file: %s\n", filepath);
			close(dbfd);
			return -1;
		}
	} else {
		dbfd = open_db_file(filepath);
		if (dbfd == STATUS_ERROR) {
			fprintf(stderr, "Error opening database file: %s\n", filepath);
			return -1;
		}

		if (validate_db_header(dbfd, &dbhdr) == STATUS_ERROR) {
			fprintf(stderr, "Error validating database header in file: %s\n", filepath);
			close(dbfd);
			return -1;
		}
	}

	load_employee_data(dbfd, dbhdr, &employees);
	poll_loop(port, dbhdr, &employees, dbfd);

    return 0;
}

