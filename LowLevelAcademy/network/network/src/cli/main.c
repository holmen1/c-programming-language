#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "common.h"

int send_hello(int fd);
int send_employee(int fd, char *addstring);
int delete_employee(int fd, char *name);
int list_employees(int fd);

void print_usage(char *argv[]) {
    printf("Usage: %s -h <host> -p <port> [-a <addarg>]\n", argv[0]);
	printf("Options:\n");
	printf("\t -h <host>   (required) host to connect to\n");
	printf("\t -p <port>   (required) port to connect to\n");
	printf("\t -a <addarg> Add a new employee with the given string format 'name,address,hours'\n");
	printf("\t -l List all employees in the database\n");
	printf("\t -d <name>    Delete employee with the given name\n");
	exit(1);
}

int main(int argc, char *argv[]) {
	char *portarg = NULL;
	char *hostarg = NULL;
	char *addarg = NULL;
	char *deletestring = NULL;
	bool list = false;
	unsigned short port = 0;
	
	int c;
	while ((c = getopt(argc, argv, "h:p:a:ld:")) != -1) {
		switch (c) {
			case 'h':
				hostarg = optarg;
				break;
			case 'p':
				portarg = optarg;
				port = atoi(portarg);
				break;
			case 'a':
				addarg = optarg;
				break;
			case 'l':
				list = true;
				break;
			case 'd':
				deletestring = optarg;
				break;
			case '?':
				fprintf(stderr, "Unknown option -%c\n", c);
				print_usage(argv);
				return -1;
			default:
				return -1;

		}
	}

	if (hostarg == NULL || portarg == NULL) {
		print_usage(argv);
		return -1;
	}

    struct sockaddr_in serverInfo = {0};

    serverInfo.sin_family = AF_INET;
    serverInfo.sin_addr.s_addr = inet_addr(hostarg);
    serverInfo.sin_port = htons(port);

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd == -1) {
        perror("socket");
        return 0;
    }

    if (connect(fd, (struct sockaddr*)&serverInfo, sizeof(serverInfo)) == -1) {
        perror("connect");
        close(fd);
        return 0;
    }

    if (send_hello(fd) != STATUS_SUCCESS) {
		close(fd);
		return -1;
	}

	if (addarg) {
		send_employee(fd, addarg);
	}

	if (deletestring) {
		delete_employee(fd, deletestring);
	}

	if (list) {
		list_employees(fd);
	}

    close(fd);
    return 0;
}


int send_hello(int fd) {
    char buf[4096] = {0};

	dbproto_hdr_t *hdr = (dbproto_hdr_t *)buf;
	hdr->type = MSG_HELLO_REQ;
	hdr->len = 1;

	dbproto_hello_req *hello_req = (dbproto_hello_req *)&hdr[1];
	hello_req->proto = PROTO_VER;

	hdr->type = htonl(hdr->type);
	hdr->len = htons(hdr->len);
	hello_req->proto = htons(hello_req->proto);

	write(fd, buf, sizeof(dbproto_hdr_t) + sizeof(dbproto_hello_req));
	read(fd, buf, sizeof(buf));

	hdr->type = ntohl(hdr->type);
	hdr->len = ntohs(hdr->len);

	if (hdr->type != MSG_HELLO_RESP) {
		fprintf(stderr, "Unexpected response type: %d\n", hdr->type);
		return STATUS_ERROR;
	}
	printf("Received hello response from server. Protocol v%d\n", ntohs(hello_req->proto));
	
	return STATUS_SUCCESS;
}

int send_employee(int fd, char *addstring) {
    char buf[4096] = {0};

	dbproto_hdr_t *hdr = (dbproto_hdr_t *)buf;
	hdr->type = MSG_EMPLOYEE_ADD_REQ;
	hdr->len = 1;

	dbproto_employee_add_req *employee = (dbproto_employee_add_req *)&hdr[1];
	strncpy((char *)employee->data, addstring, sizeof(employee->data) - 1);
	employee->data[sizeof(employee->data) - 1] = '\0'; /* proper null-termination */

	hdr->type = htonl(hdr->type);
	hdr->len = htons(hdr->len);

	write(fd, buf, sizeof(dbproto_hdr_t) + sizeof(dbproto_employee_add_req));
	printf("Sent add request to server. Employee:%s\n", addstring);
	read(fd, buf, sizeof(buf));

	hdr->type = ntohl(hdr->type);
	hdr->len = ntohs(hdr->len);

	if (hdr->type != MSG_EMPLOYEE_ADD_RESP) {
		fprintf(stderr, "Unexpected response type: %d\n", hdr->type);
		return STATUS_ERROR;
	}
	printf("Received add response from server\n");
	return STATUS_SUCCESS;
}

int delete_employee(int fd, char *name) {
    char buf[4096] = {0};

	dbproto_hdr_t *hdr = (dbproto_hdr_t *)buf;
	hdr->type = MSG_EMPLOYEE_DEL_REQ;
	hdr->len = 1;

	dbproto_employee_delete_req *employee = (dbproto_employee_delete_req *)&hdr[1];
	strncpy((char *)employee->name, name, sizeof(employee->name) - 1);
	employee->name[sizeof(employee->name) - 1] = '\0'; /* proper null-termination */

	hdr->type = htonl(hdr->type);
	hdr->len = htons(hdr->len);

	write(fd, buf, sizeof(dbproto_hdr_t) + sizeof(dbproto_employee_delete_req));
	printf("Sent delete request to server. Employee:%s\n", name);
	read(fd, buf, sizeof(buf));

	hdr->type = ntohl(hdr->type);
	hdr->len = ntohs(hdr->len);

	if (hdr->type != MSG_EMPLOYEE_DEL_RESP) {
		fprintf(stderr, "Unexpected response type: %d\n", hdr->type);
		return STATUS_ERROR;
	}
	printf("Received delete response from server\n");
	return STATUS_SUCCESS;
}

int list_employees(int fd) {
    char buf[4096] = {0};
	dbproto_hdr_t *hdr = (dbproto_hdr_t *)buf;
	hdr->type = MSG_EMPLOYEE_LIST_REQ;
	hdr->len = 0;

	hdr->type = htonl(hdr->type);
	hdr->len = htons(hdr->len);
	write(fd, buf, sizeof(dbproto_hdr_t));
	printf("Sent employee list request to server\n");

	read(fd, hdr, sizeof(dbproto_hdr_t));
	hdr->type = ntohl(hdr->type);
	hdr->len = ntohs(hdr->len);
	if (hdr->type == MSG_ERROR) {
		printf("Unable to list employees.\n");
		close(fd);
		return STATUS_ERROR;
	} 

	if (hdr->type == MSG_EMPLOYEE_LIST_RESP) {
		int count = hdr->len;
		printf("Received employee list response from server. Count: %d\n", count);

		dbproto_employee_list_resp *employee = (dbproto_employee_list_resp *)&hdr[1];
		int i = 0;
		for (; i < count; i++) {
			read(fd, employee, sizeof(dbproto_employee_list_resp));
			employee->hours = ntohl(employee->hours);
			printf("%s, %s, %d\n", employee->name, employee->address, employee->hours);			
		}
	}

	return STATUS_SUCCESS;
}