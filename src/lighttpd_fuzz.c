/* lighttppd_fuzz.c - Simple fuzzer targetting lighttpd */
/********************************************************

README:

Compiled against Spike 2.9 (Circa 2003 but I got this version in early 2013)

To compile this, I added this to the SPIKE Makefile


lighttpd_fuzz: lighttpd_fuzz.c
   $(CC) $(INCLUDE) $(SPIKE_OBS) lighttpd_fuzz.c -o lighttpd_fuzz


There were some library issues that needed to be fixed up for SPIKE 
compilation at all, but no additional ones needed once SPIKE was working.


Note: This version of lighttpd_fuzz doesn't really do anything, it's just a
basic proof of concept that I can build from.

**********************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "spike.h"
#include "tcpstuff.h"


int main (int argc, char **argv) {
	struct spike * spike_instance;
	int port;
	char *host;
	char buffer[1500000];

	/* Get some parameters */
	if (argc != 3) {
		printf("Usage: ./lighttpd_fuzz <host> <port>\n");
		exit(2);
	}

	host = argv[1];
	port = atoi(argv[2]);

	if (port < 1) {
		fprintf(stderr, "Invalid port %d, using default of 9999\n", port);
		port = 9999;
	}

	/* Set up Spike */
	spike_instance = new_spike();

	if (spike_instance == NULL) {
		fprintf(stderr, "Malloc failed trying to allocate a spike.\n");
		exit(-1);
	}

	setspike(spike_instance); 



	/* Print something so it's clear that we've started */
	printf("Spike initialized\n");	


	/* Initialize the fuzzing and reset the fuzz variables */
	s_init_fuzzing();
	s_resetfuzzvariable();
	
	/* The original generic_send_tcp had some nice ways to shortcut
      in to specific variables.  I'm skipping that for now to better
      learn how this works */

	while (!s_didlastvariable()) {
		s_resetfuzzstring();

		while(!s_didlastfuzzstring()) {

			spike_clear();

			/* Connect via TCP */
			spike_connect_tcp(host, port);
			if (spike_send() < 0) {
				fprintf(stderr, "Could not send data \n");
			}
		
		
			/* Do some stuff: This is the core commands of the fuzz script */
		
			s_readline(); //print received line from server
			s_string("GET ");
			s_string_variable("/cgi.pl");
			s_string(" HTTP/1.0");
			s_string("\n");
			s_string_variable("COMMAND"); //send fuzzed string
		
			spike_close_tcp();

	//printf("%s", s_get_databuf());

    /*see, the thing is that the spike is not guaranteed to be
            null terminated, so just a plain printf on the
            s_get_databuf() is ill-advised.*/
	     memset(buffer,0x00,sizeof(buffer));
	     if (s_get_size()>2500)
	       memcpy(buffer,s_get_databuf(),2500);
	     else
	       memcpy(buffer,s_get_databuf(),s_get_size());
	
			printf("Request:\n%.2500s\nEndRequest\n",buffer); 

			s_incrementfuzzstring();
		} /* while !s_didlastfuzzstring() */

		s_incrementfuzzvariable();
	} /* while !s_didlastvariable() */

	return 0;
}
