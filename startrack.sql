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


--
-- TOC entry 3360 (class 0 OID 218973)
-- Dependencies: 211
-- Data for Name: Series; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (1, 'Frieren', 2020, 3, 'The story follows elven mage Frieren, a former member of the party of adventurers who defeated the Demon King and restored harmony to the world after a ten-year quest. In the past, the heroic group included Frieren, human hero Himmel, dwarven warrior Eisen and human priest Heiter. Before they part, they observe the Era Meteors together, a meteor shower that occurs once in fifty years. Frieren agrees to see them again and offers them a better view the next time the celestial event occurs. Frieren then departs and travels the world in pursuit of magical knowledge.', 'Comic', 'https://upload.wikimedia.org/wikipedia/en/6/60/Frieren_Beyond_Journey%27s_End.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (2, 'The Promised Neverland', 2016, 3, 'In a strange world filled with sentient creatures of different species, an agreement called "The Promise" was made to end a long war between humans and the so-called demons. "The Promise" was an agreement where each would live in their own separate "worlds": the human world, free from the threat of demons; and the demon world, where human breeding farms were set up to provide food for the demons. By eating humans, demons take on their attributes which prevent them from degenerating into mindless monsters. In the demon world, a special breeding program was set up under the guise of orphanages; there, a human "Mother" would oversee the children to make sure they grew up as intelligent as possible. These children had identifying numbers tattooed on them and had no knowledge of the outside world. They believed that they were orphans and once they reached a certain age or intelligence, they would be taken out for adoption, but were fed to high-ranking demons instead.', 'Comic', 'https://upload.wikimedia.org/wikipedia/en/4/44/The_Promised_Neverland%2C_Volume_1.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (3, 'Attack on Titan', 2009, 4, 'Eren Yeager is a boy who lives in the town of Shiganshina, located on the outermost of three circular walls which protect their inhabitants from Titans. In the year 845, the first wall (Wall Maria) is breached by two new types of Titans, the Colossal Titan and the Armored Titan. During the incident, Eren''s mother is eaten by a Smiling Titan while Eren escapes. He swears revenge on all Titans and enlists in the military along with his childhood friends Mikasa Ackerman and Armin Arlert.', 'Comic', 'https://upload.wikimedia.org/wikipedia/en/d/d6/Shingeki_no_Kyojin_manga_volume_1.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (4, 'Sailor Moon', 1991, 4, 'One day in Juban, Tokyo, a middle-school student named Usagi Tsukino befriends Luna, a talking black cat who gives her a magical brooch enabling her to transform into Sailor Moon: a guardian destined to save Earth from the forces of evil. Luna and Usagi assemble a team of fellow Sailor Guardians to find their princess and the Silver Crystal. They encounter the studious Ami Mizuno, who awakens as Sailor Mercury; Rei Hino, a local Shinto shrine maiden who awakens as Sailor Mars; Makoto Kino, a tall and strong transfer student who awakens as Sailor Jupiter; and Minako Aino, a young aspiring idol who had awakened as Sailor Venus a few months prior, accompanied by her talking feline companion Artemis. Additionally, they befriend Mamoru Chiba, a high school student who assists them on occasion as Tuxedo Mask.', 'Comic', 'https://upload.wikimedia.org/wikipedia/en/e/e5/SMVolume1.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (5, 'Wotakoi: Love is Hard for Otaku', 2014, 5, 'The main characters are Narumi, an office working woman who hides her fujoshi lifestyle, and Hirotaka, a handsome and capable company man who is a game otaku. The two seem perfect for each other, but love is difficult for otaku.', 'Comic', 'https://upload.wikimedia.org/wikipedia/id/b/b8/Otaku_ni_Koi_wa_Muzukashii%2C_Volume_1.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (6, 'Howl''s Moving Castle', 2004, 5, 'Sophie, a young milliner and eldest of three sisters, encounters a wizard named Howl on her way to visit her sister Lettie. Upon returning home, she meets the Witch of the Waste, who transforms her into a 90-year-old woman. Seeking to break the curse, Sophie leaves home and sets off through the countryside. She meets a living scarecrow, whom she calls "Turnip Head". He leads her to Howl''s moving castle where she enters without invitation. She subsequently meets Howl''s young apprentice Markl and a fire demon named Calcifer, the source of the castle''s magic and movement. Calcifer makes a deal with Sophie, agreeing to break her curse if she breaks his link with Howl. When Howl appears, Sophie announces that she has "hired herself" as a cleaning lady.', 'Film', 'https://upload.wikimedia.org/wikipedia/en/a/a0/Howls-moving-castleposter.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (7, 'Flipped', 2010, 5, 'In 1957, 7-year-old Bryce Loski moves in next door to Julianna "Juli" Baker. Looking into each other''s eyes, Juli knows it''s love, but Bryce is unsettled and avoids her. Four years later Juli is still completely enamored, rarely leaving Bryce alone. Desperate to get rid of her, he pretends to date Sherry Stalls, a popular girl she dislikes. Bryce''s best friend Garrett tells Sherry the truth, so she dumps him. Embarrassed, Bryce hopes that in upcoming seventh grade Juli will finally meet someone else.', 'Film', 'https://upload.wikimedia.org/wikipedia/en/3/3a/Flipped_poster.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (8, 'Harry Potter and the Philosopher''s Stone', 2001, 5, 'Harry Potter lives with his abusive uncle and aunt, Vernon and Petunia Dursley, and their bullying son, Dudley. On Harry''s eleventh birthday, Rubeus Hagrid, a half-giant, delivers an acceptance letter from Hogwarts School of Witchcraft and Wizardry, revealing that Harry''s parents, James and Lily Potter, were wizards. When Harry was just a year old, a powerful but malevolent dark wizard named Lord Voldemort murdered his parents; Harry survived Voldemort''s killing curse that rebounded and seemingly destroyed the Dark Lord, leaving a lightning bolt-shaped scar on his forehead. Unknown to Harry, this act made him famous in the wizarding world.', 'Film', 'https://upload.wikimedia.org/wikipedia/en/thumb/7/7a/Harry_Potter_and_the_Philosopher%27s_Stone_banner.jpg/330px-Harry_Potter_and_the_Philosopher%27s_Stone_banner.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (9, 'The Secret of Moonacre', 2008, 3, 'Maria Merryweather inherits a book after her father''s death: The Ancient Chronicles of Moonacre Valley. She reads of the first Moon Princess receiving magical pearls from the moon. At her wedding with a Merryweather, her father, a de Noir, presented the couple with a black lion, while the groom gifted his bride a unicorn. When the princess revealed the pearls, the two families were possessed by greed.', 'Film', 'https://upload.wikimedia.org/wikipedia/en/7/7f/Secretmoonacre.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (10, 'Me Before You', 2016, 4, 'Louisa "Lou" Clark is hired as carer for Will Traynor, a once successful banker and active sportsman now tetraplegic after being hit by a motorcycle. Will''s mother hopes Lou''s bubbly personality will lift Will''s depressed and cynical spirits. Will is initially cold towards Lou. Will''s ex-girlfriend Alicia visits and reveals that she is to marry Will''s former best friend Rupert. Lou perseveres with Will and, as the two grow close, she learns he is cultured and worldly, in contrast to her simple life spent with her parents or boyfriend Patrick.', 'Film', 'https://upload.wikimedia.org/wikipedia/en/f/fd/Me_Before_You_%28film%29.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (11, 'Gen V', 2023, 4, 'At the Godolkin University School of Crimefighting, founded by Thomas Godolkin, young adult superheroes ("supes") put their moral boundaries to the test by competing for the university''s top ranking and a chance to join The Seven, Vought International''s elite superhero team. When the school''s dark secrets come to light, they must decide what kind of heroes they want to become.', 'TVSeries', 'https://m.media-amazon.com/images/M/MV5BMGYxOWMxNjMtNjE3Mi00YTRjLTlkZWUtNDk4NzBkMWEyYWE0XkEyXkFqcGdeQXVyMTUzMTg2ODkz._V1_FMjpg_UX1000_.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (12, 'The Witcher', 2019, 3, 'The story begins with Geralt of Rivia, Crown Princess Cirilla of Cintra, and the quarter-elf sorceress Yennefer of Vengerberg at different points in time, exploring formative events that shape their characters throughout the first season, before eventually merging into a single timeline.', 'TVSeries', 'https://m.media-amazon.com/images/M/MV5BMDEwOWVlY2EtMWI0ZC00OWVmLWJmZGItYTk3YjYzN2Y0YmFkXkEyXkFqcGdeQXVyMTUzMTg2ODkz._V1_.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (13, 'A Time Called You', 2023, 3, 'A Time Called You is a story about Jun-hee, who is still grieving the loss of her boyfriend Yeon-jun in the year 2023, a year after his passing. She somehow travels back in time to 1998 and wakes up inhabiting the body of a different person, 18-year-old Min-ju. As she navigates this new reality, she meets Si-heon, who bears an uncanny resemblance to her deceased boyfriend, adding to the emotional complexity of her journey.[', 'TVSeries', 'https://upload.wikimedia.org/wikipedia/en/d/dd/A_Time_Called_You.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (14, 'The Lord of the Rings: The Rings of Power', 2022, 4, 'Epic drama set thousands of years before the events of J.R.R. Tolkien''s ''The Hobbit'' and ''The Lord of the Rings'' follows an ensemble cast of characters, both familiar and new, as they confront the long-feared re-emergence of evil to Middle-earth.', 'TVSeries', 'https://m.media-amazon.com/images/M/MV5BNTg3NjcxYzgtYjljNC00Y2I2LWE3YmMtOTliZTkwYTE1MmZiXkEyXkFqcGdeQXVyNTY4NDc5MDE@._V1_.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (15, 'Lockwood & Co.', 2023, 2, 'In an alternate version of present-day Britain, ghosts who are deadly to the touch have been rising from their graves for the past 50 years. Because of this phenomenon, known as "The Problem", technological advances have stopped; for example, there is no internet and people still use cassette recorders.', 'TVSeries', 'https://upload.wikimedia.org/wikipedia/en/7/73/Lockwood_and_Co_poster.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (16, 'It Ends with Us', 2016, 5, 'College graduate Lily Bloom moves to Boston with hopes of opening her own floral shop. She has recently given the eulogy at her father''s funeral, in her hometown of Plethora, Maine. Her father was abusive towards her mother, who kept the abuse a secret, leading Lily to resent both of them. She reads through her old childhood diaries and remembers her first love Atlas Corrigan, who left to join the military, but promised to return to her.', 'Novel', 'https://upload.wikimedia.org/wikipedia/en/e/e4/It_Ends_with_Us_%28Colleen_Hoover%29.png');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (17, 'Graceling', 2008, 5, 'Graceling takes place in a world in which people with special powers are knowns as Gracelings. Gracelings are identified when their eyes become two different colors. In the Middluns, Gracelings are put in the service of the king. Katsa is a young woman known for her Grace of killing. She has been in the service of her uncle, King Randa, since she was a child, tasked with executing or torturing those who oppose or displease him. She also runs the secret "Council", which aims for justice in the Seven Kingdoms.', 'Novel', 'https://upload.wikimedia.org/wikipedia/en/8/85/Graceling_cover.png');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (18, 'The Poppy War', 2018, 4, 'Rin is a poor war orphan who lives with opium seller foster parents. To avoid an arranged marriage, she secretely studies for a national test, the Keju, as an escape. Much to her surprise and the villagers'' Rin places first and is sent off to a Sinegard, the imperial military academy in the north. Although the students mock the color of her skin and her southern accent, Rin excels in her studies. She becomes friends with Kitay, the son of a minister, and enemies with Nezha, the son of warlord, and looks up to Altan, the school''s star student. At the end of her first year she discovers her talent for shamanism and spends the next year studying with the batty Master Jiang, who teaches her how to access the magic of the gods via the use of meditation and more importantly psychedelic drugs.', 'Novel', 'https://upload.wikimedia.org/wikipedia/en/c/ca/The_Poppy_War_1st_cover.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (19, 'Cantik itu Luka', 2002, 5, 'Di akhir masa kolonial, seorang perempuan dipaksa menjadi pelacur. Kehidupan itu terus dijalaninya hingga ia memiliki tiga anak gadis yang kesemuanya cantik. Ketika mengandung anaknya yang keempat, ia berharap anak itu akan lahir buruk rupa. Itulah yang terjadi, meskipun secara ironik ia memberinya nama si Cantik.', 'Novel', 'https://upload.wikimedia.org/wikipedia/id/d/d2/CiL_%28sampul%29.jpg');
INSERT INTO public."Series" OVERRIDING SYSTEM VALUE VALUES (20, 'Little Women', 1868, 5, 'Four sisters and their mother, whom they call Marmee, live in a new neighborhood (loosely based on Concord) in Massachusetts in genteel poverty. Having lost all his money, their father is serving as a chaplain for the Union Army in the American Civil War, far from home. The mother and daughters face their first Christmas without him. When Marmee asks her daughters to give their Christmas breakfast away to an impoverished family, the girls and their mother venture into town laden with baskets to feed the hungry children. When they return, they discover their wealthy, elderly neighbor Mr. Laurence has sent over a decadent surprise dinner to make up for their breakfast. The two families become acquainted following these acts of kindness.', 'Novel', 'https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Houghton_AC85.A%E2%84%93194L.1869_pt.2aa_-_Little_Women%2C_title.jpg/300px-Houghton_AC85.A%E2%84%93194L.1869_pt.2aa_-_Little_Women%2C_title.jpg');

--
-- TOC entry 3362 (class 0 OID 218981)
-- Dependencies: 213
-- Data for Name: User; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public."User" VALUES ('admin', '$2b$10$hBGVNC1gtdsXIvxLNTYtEe9qvCM/Fr6O02/gUCrsdAilamy82f3T.', 'admin', 'ADMIN');

--
-- TOC entry 3373 (class 0 OID 0)
-- Dependencies: 212
-- Name: Series_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('public."Series_id_seq"', 20, true);


--
-- TOC entry 3374 (class 0 OID 0)
-- Dependencies: 217
-- Name: UserSeries_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('public."UserSeries_id_seq"', 1, false);

-- Completed on 2023-12-10 16:27:28

--
-- PostgreSQL database dump complete
--