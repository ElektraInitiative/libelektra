// clang-format off
ksNew (16,
	keyNew (PREFIX "/emojis",
		KEY_VALUE, "😀😅😇🥰🤪🤐🤥🤢🙆🙋🚶‍♀️🏃🍟⚓🔆📶◾",
		KEY_META, "type", "string",
		KEY_META, "order", "0", 
	KEY_END),

	keyNew (PREFIX "/utf8_basic_multiline",
		KEY_VALUE,
			"Runes: ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ\n"
			"ᛋᚳᛖᚪᛚ᛫ᚦᛖᚪᚻ᛫ᛗᚪᚾᚾᚪ᛫ᚷᛖᚻᚹᛦᛚᚳ᛫ᛗᛁᚳᛚᚢᚾ᛫ᚻᛦᛏ᛫ᛞᚫᛚᚪᚾ\n"
			"ᚷᛁᚠ᛫ᚻᛖ᛫ᚹᛁᛚᛖ᛫ᚠᚩᚱ᛫ᛞᚱᛁᚻᛏᚾᛖ᛫ᛞᚩᛗᛖᛋ᛫ᚻᛚᛇᛏᚪᚾ᛬",
		KEY_META, "type", "string",
		KEY_META, "order", "1",
		KEY_META, "comment/#1", " following samples are taken from http://kermitproject.org/utf8.html",
		KEY_META, "comment/#1/start", "#",
		KEY_META, "comment/#1/space", "0",
	KEY_END),

	keyNew (PREFIX "/utf8_literal_multiline",
		KEY_VALUE,
			"Middle English: An preost wes on leoden, Laȝamon was ihoten\n"
			"He wes Leovenaðes sone -- liðe him be Drihten.\n"
			"He wonede at Ernleȝe at æðelen are chirechen,\n"
			"Uppen Sevarne staþe, sel þar him þuhte,\n"
			"Onfest Radestone, þer he bock radde.",
		KEY_META, "type", "string",
		KEY_META, "order", "2",
	KEY_END),

	keyNew (PREFIX "/utf8_literal",
		KEY_VALUE,
			"Middle High German: Sîne klâwen durh die wolken sint geslagen, "
			"er stîget ûf mit grôzer kraft, ich sih in grâwen tägelîch als er wil tagen, "
			"den tac, der im geselleschaft erwenden wil, dem werden man, den ich mit sorgen în verliez.  "
			"ich bringe in hinnen, ob ich kan.  sîn vil manegiu tugent michz leisten hiez.",
		KEY_META, "type", "string",
		KEY_META, "order", "3",
	KEY_END),

	keyNew (PREFIX "/utf8_basic",
		KEY_VALUE,
			"Greek Monotonic: Τη γλώσσα μου έδωσαν ελληνική το σπίτι φτωχικό στις α"
			"μμουδιές του Ομήρου.Μονάχη έγνοια η γλώσσα μου στις αμμουδιές του Ομήρ"
			"ου.από το Άξιον Εστί του Οδυσσέα Ελύτη",
		KEY_META, "type", "string",
		KEY_META, "order", "4",
	KEY_END),

	keyNew (PREFIX "/in_keyname (russian)/На берегу пустынных волн/Стоял он, дум великих полн",
		KEY_VALUE,
			"И вдаль глядел. Пред ним широко\n"
			"Река неслася; бедный чёлн\n"
			"По ней стремился одиноко.\n"
			"По мшистым, топким берегам\n"
			"Чернели избы здесь и там,\n"
			"Приют убогого чухонца;\n"
			"И лес, неведомый лучам\n"
			"В тумане спрятанного солнца,\n"
			"Кругом шумел.",
		KEY_META, "origvalue",
			"\nИ вдаль глядел. Пред ним широко\n"
			"Река неслася; бедный чёлн\n"
			"По ней стремился одиноко.\n"
			"По мшистым, топким берегам\n"
			"Чернели избы здесь и там,\n"
			"Приют убогого чухонца;\n"
			"И лес, неведомый лучам\n"
			"В тумане спрятанного солнца,\n"
			"Кругом шумел.",
		KEY_META, "type", "string",
		KEY_META, "order", "5",
		KEY_META, "comment/#1",
			  " Greek Polytonic: Τὴ γλῶσσα μοῦ ἔδωσαν ἑλληνικὴ τὸ σπίτι φτωχικὸ στὶς "
			  "ἀμμουδιὲς τοῦ Ὁμήρου.  Μονάχη ἔγνοια ἡ γλῶσσα μου στὶς ἀμμουδιὲς τοῦ "
			  "Ὁμήρου.  ἀπὸ τὸ Ἄξιον ἐστί τοῦ Ὀδυσσέα Ἐλύτη",
		KEY_META, "comment/#1/start", "#",
		KEY_META, "comment/#1/space", "0",
	KEY_END),

	keyNew (PREFIX "/tamil",
		KEY_VALUE,
			"யாமறிந்த மொழிகளிலே தமிழ்மொழி போல் இனிதாவது எங்கும் காணோம்,\n"
			"பாமரராய் விலங்குகளாய், உலகனைத்தும் இகழ்ச்சிசொலப் பான்மை கெட்டு,\n"
			"நாமமது தமிழரெனக் கொண்டு இங்கு வாழ்ந்திடுதல் நன்றோ? சொல்லீர்!\n"
			"தேமதுரத் தமிழோசை உலகமெலாம் பரவும்வகை செய்தல் வேண்டும்",
		KEY_META, "type", "string",
		KEY_META, "order", "6",
	KEY_END),

	keyNew (PREFIX "/kannada",
		KEY_VALUE,
			"ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸು ಇಂದೆನ್ನ ಹೃದಯದಲಿ\n"
			"ನಿತ್ಯವೂ ಅವತರಿಪ ಸತ್ಯಾವತಾರ\n\n"
			"ಮಣ್ಣಾಗಿ ಮರವಾಗಿ ಮಿಗವಾಗಿ ಕಗವಾಗೀ...\n"
			"ಮಣ್ಣಾಗಿ ಮರವಾಗಿ ಮಿಗವಾಗಿ ಕಗವಾಗಿ\n"
			"ಭವ ಭವದಿ ಭತಿಸಿಹೇ ಭವತಿ ದೂರ\n"
			"ನಿತ್ಯವೂ ಅವತರಿಪ ಸತ್ಯಾವತಾರ || ಬಾ ಇಲ್ಲಿ || ",
		KEY_META, "type", "string",
		KEY_META, "order", "7",
	KEY_END),

	keyNew (PREFIX "/in_table/georgian/ვეპხის",
		KEY_META, "order", "8",
		KEY_META, "tomltype", "simpletable",
	KEY_END),

	keyNew (PREFIX "/in_table/georgian/ვეპხის/more_georgian",
		KEY_VALUE,
			"ტყაოსანი შოთა რუსთაველი ღმერთსი შემვედრე, ნუთუ კვლა დამხსნა"
			"ს სოფლისა შრომასა, ცეცხლს, წყალსა და მიწასა, ჰაერთა თანა მრ"
			"ომასა; მომცნეს ფრთენი და აღვფრინდე, მივჰხვდე მას ჩემსა ნდომ"
			"ასა, დღისით და ღამით ვჰხედვიდე მზისა ელვათა კრთომაასა.",
		KEY_META, "type", "string",
		KEY_META, "order", "9",
	KEY_END),

	keyNew (PREFIX "/in_table_array/sanskrit/ ﻿काचं शक्नोम्यत्तुम् । नोपहिनस्ति माम् ॥ ",
		KEY_META, "order", "10",
		KEY_META, "tomltype" , "tablearray",
		KEY_META, "array", "#0",
	KEY_END),

KS_END)
