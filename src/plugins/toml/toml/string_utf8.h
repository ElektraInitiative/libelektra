// clang-format off
ksNew (16,
	keyNew (PREFIX "/emojis",
		ELEKTRA_KEY_VALUE, "😀😅😇🥰🤪🤐🤥🤢🙆🙋🚶‍♀️🏃🍟⚓🔆📶◾",
		ELEKTRA_KEY_META, "tomltype", "string_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "0", 
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/utf8_basic_multiline",
		ELEKTRA_KEY_VALUE,
			"Runes: ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ\n"
			"ᛋᚳᛖᚪᛚ᛫ᚦᛖᚪᚻ᛫ᛗᚪᚾᚾᚪ᛫ᚷᛖᚻᚹᛦᛚᚳ᛫ᛗᛁᚳᛚᚢᚾ᛫ᚻᛦᛏ᛫ᛞᚫᛚᚪᚾ\n"
			"ᚷᛁᚠ᛫ᚻᛖ᛫ᚹᛁᛚᛖ᛫ᚠᚩᚱ᛫ᛞᚱᛁᚻᛏᚾᛖ᛫ᛞᚩᛗᛖᛋ᛫ᚻᛚᛇᛏᚪᚾ᛬",
		ELEKTRA_KEY_META, "tomltype", "string_ml_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "1",
		ELEKTRA_KEY_META, "comment/#1", " following samples are taken from http://kermitproject.org/utf8.html",
		ELEKTRA_KEY_META, "comment/#1/start", "#",
		ELEKTRA_KEY_META, "comment/#1/space", "",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/utf8_literal_multiline",
		ELEKTRA_KEY_VALUE,
			"Middle English: An preost wes on leoden, Laȝamon was ihoten\n"
			"He wes Leovenaðes sone -- liðe him be Drihten.\n"
			"He wonede at Ernleȝe at æðelen are chirechen,\n"
			"Uppen Sevarne staþe, sel þar him þuhte,\n"
			"Onfest Radestone, þer he bock radde.",
		ELEKTRA_KEY_META, "tomltype", "string_ml_literal",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "2",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/utf8_literal",
		ELEKTRA_KEY_VALUE,
			"Middle High German: Sîne klâwen durh die wolken sint geslagen, "
			"er stîget ûf mit grôzer kraft, ich sih in grâwen tägelîch als er wil tagen, "
			"den tac, der im geselleschaft erwenden wil, dem werden man, den ich mit sorgen în verliez.  "
			"ich bringe in hinnen, ob ich kan.  sîn vil manegiu tugent michz leisten hiez.",
		ELEKTRA_KEY_META, "tomltype", "string_literal",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "3",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/utf8_basic",
		ELEKTRA_KEY_VALUE,
			"Greek Monotonic: Τη γλώσσα μου έδωσαν ελληνική το σπίτι φτωχικό στις α"
			"μμουδιές του Ομήρου.Μονάχη έγνοια η γλώσσα μου στις αμμουδιές του Ομήρ"
			"ου.από το Άξιον Εστί του Οδυσσέα Ελύτη",
		ELEKTRA_KEY_META, "tomltype", "string_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "4",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/in_keyname (russian)/На берегу пустынных волн/Стоял он, дум великих полн",
		ELEKTRA_KEY_VALUE,
			"И вдаль глядел. Пред ним широко\n"
			"Река неслася; бедный чёлн\n"
			"По ней стремился одиноко.\n"
			"По мшистым, топким берегам\n"
			"Чернели избы здесь и там,\n"
			"Приют убогого чухонца;\n"
			"И лес, неведомый лучам\n"
			"В тумане спрятанного солнца,\n"
			"Кругом шумел.",
		ELEKTRA_KEY_META, "origvalue",
			"\nИ вдаль глядел. Пред ним широко\n"
			"Река неслася; бедный чёлн\n"
			"По ней стремился одиноко.\n"
			"По мшистым, топким берегам\n"
			"Чернели избы здесь и там,\n"
			"Приют убогого чухонца;\n"
			"И лес, неведомый лучам\n"
			"В тумане спрятанного солнца,\n"
			"Кругом шумел.",
		ELEKTRA_KEY_META, "tomltype", "string_ml_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "5",
		ELEKTRA_KEY_META, "comment/#1",
			  " Greek Polytonic: Τὴ γλῶσσα μοῦ ἔδωσαν ἑλληνικὴ τὸ σπίτι φτωχικὸ στὶς "
			  "ἀμμουδιὲς τοῦ Ὁμήρου.  Μονάχη ἔγνοια ἡ γλῶσσα μου στὶς ἀμμουδιὲς τοῦ "
			  "Ὁμήρου.  ἀπὸ τὸ Ἄξιον ἐστί τοῦ Ὀδυσσέα Ἐλύτη",
		ELEKTRA_KEY_META, "comment/#1/start", "#",
		ELEKTRA_KEY_META, "comment/#1/space", "",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/tamil",
		ELEKTRA_KEY_VALUE,
			"யாமறிந்த மொழிகளிலே தமிழ்மொழி போல் இனிதாவது எங்கும் காணோம்,\n"
			"பாமரராய் விலங்குகளாய், உலகனைத்தும் இகழ்ச்சிசொலப் பான்மை கெட்டு,\n"
			"நாமமது தமிழரெனக் கொண்டு இங்கு வாழ்ந்திடுதல் நன்றோ? சொல்லீர்!\n"
			"தேமதுரத் தமிழோசை உலகமெலாம் பரவும்வகை செய்தல் வேண்டும்",
		ELEKTRA_KEY_META, "tomltype", "string_ml_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "6",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/kannada",
		ELEKTRA_KEY_VALUE,
			"ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸು ಇಂದೆನ್ನ ಹೃದಯದಲಿ\n"
			"ನಿತ್ಯವೂ ಅವತರಿಪ ಸತ್ಯಾವತಾರ\n\n"
			"ಮಣ್ಣಾಗಿ ಮರವಾಗಿ ಮಿಗವಾಗಿ ಕಗವಾಗೀ...\n"
			"ಮಣ್ಣಾಗಿ ಮರವಾಗಿ ಮಿಗವಾಗಿ ಕಗವಾಗಿ\n"
			"ಭವ ಭವದಿ ಭತಿಸಿಹೇ ಭವತಿ ದೂರ\n"
			"ನಿತ್ಯವೂ ಅವತರಿಪ ಸತ್ಯಾವತಾರ || ಬಾ ಇಲ್ಲಿ || ",
		ELEKTRA_KEY_META, "tomltype", "string_ml_literal",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "7",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/in_table/georgian/ვეპხის",
		ELEKTRA_KEY_META, "order", "8",
		ELEKTRA_KEY_META, "tomltype", "simpletable",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/in_table/georgian/ვეპხის/more_georgian",
		ELEKTRA_KEY_VALUE,
			"ტყაოსანი შოთა რუსთაველი ღმერთსი შემვედრე, ნუთუ კვლა დამხსნა"
			"ს სოფლისა შრომასა, ცეცხლს, წყალსა და მიწასა, ჰაერთა თანა მრ"
			"ომასა; მომცნეს ფრთენი და აღვფრინდე, მივჰხვდე მას ჩემსა ნდომ"
			"ასა, დღისით და ღამით ვჰხედვიდე მზისა ელვათა კრთომაასა.",
		ELEKTRA_KEY_META, "tomltype", "string_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "9",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/in_table_array/sanskrit/ ﻿काचं शक्नोम्यत्तुम् । नोपहिनस्ति माम् ॥ ",
		ELEKTRA_KEY_META, "order", "10",
		ELEKTRA_KEY_META, "tomltype" , "tablearray",
		ELEKTRA_KEY_META, "array", "#0",
	ELEKTRA_KEY_END),

ELEKTRA_KS_END)
