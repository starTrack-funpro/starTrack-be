--
-- PostgreSQL database dump
--

-- Dumped from database version 14.5
-- Dumped by pg_dump version 14.5

-- Started on 2023-12-09 12:09:42

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

DROP DATABASE startrack;
--
-- TOC entry 3363 (class 1262 OID 202646)
-- Name: startrack; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE startrack WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'English_United States.1252';


ALTER DATABASE startrack OWNER TO postgres;

\connect startrack

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 836 (class 1247 OID 202667)
-- Name: SeriesType; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public."SeriesType" AS ENUM (
    'TVSeries',
    'Film',
    'Comic',
    'Novel'
);


ALTER TYPE public."SeriesType" OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- TOC entry 214 (class 1259 OID 207365)
-- Name: Chapter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Chapter" (
    no integer NOT NULL,
    "pageFrom" integer DEFAULT 0 NOT NULL,
    "pageTo" integer DEFAULT 0 NOT NULL,
    title character varying NOT NULL,
    "seriesId" integer NOT NULL
);


ALTER TABLE public."Chapter" OWNER TO postgres;

--
-- TOC entry 215 (class 1259 OID 207391)
-- Name: Episode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Episode" (
    no integer NOT NULL,
    title character varying NOT NULL,
    duration time without time zone NOT NULL,
    "seriesId" integer NOT NULL
);


ALTER TABLE public."Episode" OWNER TO postgres;

--
-- TOC entry 210 (class 1259 OID 202654)
-- Name: Series; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Series" (
    id integer NOT NULL,
    title character varying NOT NULL,
    year integer NOT NULL,
    rating double precision DEFAULT 0 NOT NULL,
    description character varying NOT NULL,
    type public."SeriesType" NOT NULL,
    "imageUrl" character varying DEFAULT ''::character varying NOT NULL
);


ALTER TABLE public."Series" OWNER TO postgres;

--
-- TOC entry 211 (class 1259 OID 202665)
-- Name: Series_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public."Series" ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public."Series_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- TOC entry 209 (class 1259 OID 202647)
-- Name: User; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."User" (
    username character varying NOT NULL,
    password character varying NOT NULL,
    name character varying NOT NULL,
    role character varying DEFAULT 'USER'::character varying
);


ALTER TABLE public."User" OWNER TO postgres;

--
-- TOC entry 217 (class 1259 OID 207437)
-- Name: UserChapter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."UserChapter" (
    "user" character varying NOT NULL,
    "chapterNo" integer NOT NULL,
    "seriesId" integer NOT NULL,
    "lastReadPage" integer NOT NULL
);


ALTER TABLE public."UserChapter" OWNER TO postgres;

--
-- TOC entry 216 (class 1259 OID 207420)
-- Name: UserEpisode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."UserEpisode" (
    "user" character varying NOT NULL,
    "seriesId" integer NOT NULL,
    "episodeNo" integer NOT NULL,
    "lastWatchTime" time without time zone NOT NULL
);


ALTER TABLE public."UserEpisode" OWNER TO postgres;

--
-- TOC entry 212 (class 1259 OID 202675)
-- Name: UserSeries; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."UserSeries" (
    id integer NOT NULL,
    username character varying NOT NULL,
    "seriesId" integer NOT NULL
);


ALTER TABLE public."UserSeries" OWNER TO postgres;

--
-- TOC entry 213 (class 1259 OID 202693)
-- Name: UserSeries_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public."UserSeries" ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public."UserSeries_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- TOC entry 3204 (class 2606 OID 207373)
-- Name: Chapter Chapter_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Chapter"
    ADD CONSTRAINT "Chapter_pkey" PRIMARY KEY ("seriesId", no);


--
-- TOC entry 3206 (class 2606 OID 207397)
-- Name: Episode Episode_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Episode"
    ADD CONSTRAINT "Episode_pkey" PRIMARY KEY ("seriesId", no);


--
-- TOC entry 3200 (class 2606 OID 202661)
-- Name: Series Series_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Series"
    ADD CONSTRAINT "Series_pkey" PRIMARY KEY (id);


--
-- TOC entry 3210 (class 2606 OID 207443)
-- Name: UserChapter UserChapter_pkey1; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserChapter"
    ADD CONSTRAINT "UserChapter_pkey1" PRIMARY KEY ("seriesId", "chapterNo", "user");


--
-- TOC entry 3208 (class 2606 OID 207426)
-- Name: UserEpisode UserEpisode_pkey1; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserEpisode"
    ADD CONSTRAINT "UserEpisode_pkey1" PRIMARY KEY ("seriesId", "episodeNo", "user");


--
-- TOC entry 3202 (class 2606 OID 202682)
-- Name: UserSeries UserSeries_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserSeries"
    ADD CONSTRAINT "UserSeries_pkey" PRIMARY KEY (id);


--
-- TOC entry 3198 (class 2606 OID 202653)
-- Name: User User_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."User"
    ADD CONSTRAINT "User_pkey" PRIMARY KEY (username);


--
-- TOC entry 3217 (class 2606 OID 207444)
-- Name: UserChapter Chapter_UserChapter_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserChapter"
    ADD CONSTRAINT "Chapter_UserChapter_fkey" FOREIGN KEY ("chapterNo", "seriesId") REFERENCES public."Chapter"(no, "seriesId");


--
-- TOC entry 3214 (class 2606 OID 207398)
-- Name: Episode Episode_Series_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Episode"
    ADD CONSTRAINT "Episode_Series_fkey" FOREIGN KEY ("seriesId") REFERENCES public."Series"(id);


--
-- TOC entry 3215 (class 2606 OID 207427)
-- Name: UserEpisode Episode_UserEpisode_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserEpisode"
    ADD CONSTRAINT "Episode_UserEpisode_fkey" FOREIGN KEY ("episodeNo", "seriesId") REFERENCES public."Episode"(no, "seriesId");


--
-- TOC entry 3213 (class 2606 OID 207374)
-- Name: Chapter Series_Chapter_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Chapter"
    ADD CONSTRAINT "Series_Chapter_fkey" FOREIGN KEY ("seriesId") REFERENCES public."Series"(id) NOT VALID;


--
-- TOC entry 3212 (class 2606 OID 202688)
-- Name: UserSeries Series_UserSeries_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserSeries"
    ADD CONSTRAINT "Series_UserSeries_fkey" FOREIGN KEY ("seriesId") REFERENCES public."Series"(id);


--
-- TOC entry 3218 (class 2606 OID 207449)
-- Name: UserChapter User_UserChapter_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserChapter"
    ADD CONSTRAINT "User_UserChapter_fkey" FOREIGN KEY ("user") REFERENCES public."User"(username);


--
-- TOC entry 3216 (class 2606 OID 207432)
-- Name: UserEpisode User_UserEpisode_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserEpisode"
    ADD CONSTRAINT "User_UserEpisode_fkey" FOREIGN KEY ("user") REFERENCES public."User"(username);


--
-- TOC entry 3211 (class 2606 OID 202683)
-- Name: UserSeries User_UserSeries_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."UserSeries"
    ADD CONSTRAINT "User_UserSeries_fkey" FOREIGN KEY (username) REFERENCES public."User"(username);


-- Completed on 2023-12-09 12:09:42

--
-- PostgreSQL database dump complete
--

