SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

--- Almost all of the tables depend on the table 'versions'
--- and will thus be truncated at the same time.
TRUNCATE versions CASCADE;

--
-- Data for Name: versions; Type: TABLE DATA; Schema: public; Owner: gael
--

COPY versions (version_name, main, major, minor, revision, release_date, commit_id, locc) FROM stdin;
linux-2.6.32	2	6	32	\N	12/02/2009	\N	7663555
linux-2.6.32.1	2	6	32	1	12/14/2009	\N	7663646
linux-2.6.32.2	2	6	32	2	12/18/2009	\N	7664191 
linux-2.6.32.3	2	6	32	3	01/06/2009	\N	7664544 
linux-2.6.32.4	2	6	32	4	01/18/2009	\N	7664022
linux-2.6.32.5	2	6	32	5	12/02/2009	\N	7664069
linux-2.6.32.10	2	6	32	10	03/15/2010	\N	7664504 
linux-2.6.32.20	2	6	32	20	08/20/2010	\N	7668197 
linux-2.6.32.30	2	6	32	30	03/02/2011	\N	7670116
linux-2.6.32.40	2	6	32	40	05/09/2011	\N	7671401
linux-2.6.32.50	2	6	32	50	12/09/2011	\N	7672389
linux-2.6.32.60	2	6	32	60	10/07/2012	\N	7673655
\.

-- PostgreSQL database dump complete
--
