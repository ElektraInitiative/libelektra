<?php
/**
@Author: Markus Raab
@Version: homephp 1.1-custom (utf+xhtml version)
@Licence: GPL
*/
?>
<!-- test date Generated with homephp 1.1 -->
<? echo '<?xml version="1.0" encoding="UTF-8"?>'; ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title>
<? show_title()?>
	</title>
	<link rel="Shortcut Icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="StyleSheet" href="/style.css" type="text/css" />
    <link rel="stylesheet" href="docbook.css" type="text/css" />
<? show_meta()?>
</head>

<body>
	<table class="mytable" cellspacing="0" cellpadding="0" border="0">
	<colgroup>
		<col width="20" />
		<col />
	</colgroup>
	
<tbody>
<tr>
	<td class="mylogo">
		<a href="/"><img class="pic_logo" src="/logo.png" alt="Home" title="Home" /></a>
	</td>
	<td class="myheader">
<? show_position()?>
	</td>
</tr>
<tr>
	<td class="mysidebar" valign="top">
	<div class="ro"><div class="lo"><div class="ru"><div class="lu">
	<div class="mynavigator">
	<ul>
<? generate_menu()?>
	</ul>
	</div>
	</div></div></div></div>
	</td>
	<td valign="top">
	<a name="top"></a>
	<div class="mybody">
<? show_file()?><br/>
	<a class="mytop" href="#top" title="Go to top">TOP</a>
	</div>
	</td>
</tr>
<tr>
	<td class="mydate">
<?readfile ("date.txt")?>
	</td>
	<td class="mybottom">
<? generate_banner()?>
	</td>
	</tr>
</tbody>
</table>

</body>
</html>

<?php  /** PHP Funktionen
Dieses Framework ermöglicht ein Menüsystem, eine Bildervorschau
ein Newssystem und einige andere kleine Features.

Um flexibel zu bleiben und nicht bei jeder Seite die php-
Funktionen abzuändern, gibt es eine vorgeschriebene Ordner-
struktur, mit der man dann aber flexibel Inhalte auf der
Seite verteilt ausgeben kann.

Das Design wird komplett über ein Stylesheet verwaltet.

Ordnerstruktur root:
/
|
| - index.php (Dieses Skript)
| - about.html (In der Hauptseite angezeigt)
| - [greeting.html] (oben im Titel)
| - date.txt (ganz links unten)
| - standard.html (wird gezeigt, falls in einem Unterordner
|	kein index.html vorhanden ist)
| - logo.png (Bild welches rechts oben angezeigt wird)
| - style.css (Stylesheet für alle Seiten)
|
\ - pic (Ausgeblendeter Ordner für Bilder)
\ - banner (Ausgeblendeter Ordner für banner-Bilder unten)

In der root werden alle zusätzlichen Dateien ausgeblendet!

Beliebiger Ordner
|
| - index.html (Startseite wenn Ordner angewählt wird)
| - [greeting.html] (wird statt obiger Navigation angezeigt)
| - images.html (Zeigt an, wenn es sich um Bilder handelt)
| - news.html (Zeigt an, wenn es sich um eine Newsseite handelt)
| - references.html (Zeigt an, wenn es sich um Referenzen handelt)

*/

/**Gibt Dateiname zurück*/
function get_filename()
{
	$filename = $_GET['show'];

	if ($filename == "" || $filename == ".")
	{
		$filename = "about.html";
	} else if (is_dir($filename))
	{
		if (file_exists("$filename/news.html"))
			$filename .= "/news.html";
		else if (file_exists("$filename/images.html"))
			$filename .= "/images.html";
		else if (file_exists("$filename/references.html"))
			$filename .= "/references.html";
		else $filename .= "/index.html";
	}
	return $filename;
}

/**Findet heraus ob eine Extension als Text
 * interpretiert werden soll.*/
function is_txt_extension ($extension)
{
	switch ($extension) {
	case ".txt": 
	case ".c": case ".cpp":
	case ".h":
	case ".log":
	case ".sh":
	return true;
	}
	return false;
}

/**Zeigt den Titel in der Titelleiste des Browsers an.*/
function show_title()
{
	$dirname = dirname (get_filename());
	if (file_exists ("$dirname/title.txt"))
	{
		readfile ("$dirname/title.txt");
		return;
	}
	if ($pathname == "")
	{
		echo "Home\n"; 
		return;
	}
	$pathname = format_dir($dirname);
	echo $pathname;
}

function show_meta()
{
	$dirname = dirname (get_filename());
	if ($dirname != "." && file_exists ("meta.html"))
		readfile ("meta.html");
	if (file_exists ("$dirname/meta.html"))
		readfile ("$dirname/meta.html");
}


/**Zeigt die aktuelle Position rechts oben an. Wenn ein
 * greeting.html im Ordner enthalten ist, wird stattessen
 * dieses angezeigt.*/
function show_position()
{
	$filename = get_filename();
	$pathname = dirname($filename);
	$positions = split ("/", $pathname);

	if (is_file($pathname."/"."greeting.html"))
		readfile ($pathname."/"."greeting.html"); 
	else {
		echo '		<ul id="list">' . "\n";
		echo '			<li><a href="/">Home</a></li>' . "\n";
		foreach ($positions as $pos) {
			$full .= $pos;
			$full .= "/";
		echo '			<li><a href="/' .
				$full .	'">' . format_dir($pos) .
				"</a></li>\n";
		}
		echo "		</ul>\n";
	}
}

/**Datei ausgeben*/
function show_file ()
{
	$filename = get_filename();
	$pos = strpos($filename,".");
	if ($pos == false)
	{	// Wenn keine Erweiterung, nimm txt an!
		$extension = ".txt";
	} else {
		$extension = substr($filename,$pos);
	}

	if (is_txt_extension ($extension)) echo "<pre>\n";
	if (file_exists ($filename))
	{
		readfile ($filename);
	} else {
		readfile ("standard.html");
	}
	if (is_txt_extension ($extension)) echo "\n</pre>";
	
	if (basename($filename) == "images.html") {
		generate_images (dirname($filename));
	}

	if (basename($filename) == "news.html") {
		generate_news (dirname($filename));
	}
	
	if (basename($filename) == "references.html") {
		generate_references (dirname($filename));
	}
	
	echo "\n";
}

/**Gibt den Dateinamen aus*/
function show_file_name ($file, $fileselected)
{
	$name = basename($file);
	$pos = strpos($name,".");
	$isbin = false;
	if ($pos == false)
	{	// Wenn keine Erweiterung, nimm txt an!
		$extension = ".txt";
		$pre = $name;
	} else {
		$extension = substr($name,$pos);
		if ($extension == ".html" || $extension == ".txt")
			$pre = substr($name,0,$pos);
		else $pre = $name;
	}	
	$dir = dirname ($file);
	
	echo '			<li class="';
	if (basename($file) == $fileselected)
		print 'file_selected_smaller';
	else if (is_executable ($file))
	{
		print 'file_bin';
		$isbin = true;
	}
	else if ($extension == ".html") {
		print 'file_html';
	}
	else if (is_txt_extension ($extension)) {
		print 'file_txt';
	} else {
		print 'file_bin'; 
		$isbin = true; 
	}
	echo '"> <a href="/' ,
		$file . '">' ,	basename ($pre) ,
		'</a></li>', "\n";
}

/**Gibt ein formatierten Namen einer Datei zurück.
 * "Ordner/4Datei" wird zu "Datei". Die erste Zahl
 * kann deshalb zum Sortieren von Dateien und Ordner
 * verwendet werden.*/
function format_dir ($entry)
{
	$entry = basename($entry);
	if (ereg("[0-9]", $entry))
	{
		$entry = substr($entry,1);
	}
	return $entry;
}


/** generate_menu erstellt eine Liste von Dateinamen.
  * Das Menü wird über 'show' erhalten.
  *
  * Hier ist neu, dass in Apache die RewriteEngine
  * aktiviert sein muss: 
  * (folgende Zeilen in http.conf einfügen)
  *------------ cut here -------------
  * RewriteEngine on
  * Options +FollowSymlinks
  * RewriteCond %{REQUEST_URI} !\.(php|gif|jpg|png|gif|css|gz|asc|vcf).*
  * RewriteRule ^/(.*) /index.php?show=$1
  *------------ cut here -------------
  *
  * Bsp: www.example.com/Ordner
  * wird zu (durch Rewrite):
  * www.example.com/index.php?show=Ordner
  *
  * das nach show= wird dann php übergeben.
  *
  * In diesem Beispiel würde Ordner verwendet werden
  * und alle enthaltenen Dateien im Menü dargestellt
  * werden.
  *
  * Aus den Dateiname wird der Pfad gewonnen. Wenn der
  * Dateiname bereits ein Verzeichnis ist, wird index.html
  * angenommen. Einzige Ausnahme ist das root Verzeichnis.
  * Dort wird about.html angzeigt, da es sonst zu Konflikte
  * mit dem Webserver kommen k&ouml;nnte, wenn index.html +
  * index.php vorhanden ist.
  */
function generate_menu ()
{
	$filename = get_filename();
	$fileselected = basename ($filename);

	if ($fileselected == "news.html")
		$filename = dirname($filename);
	else if ($fileselected == "images.html")
		$filename = dirname($filename);
	else if ($fileselected == "references.html")
		$filename = dirname($filename);

	$pathname = dirname($filename);

	$dirs = array();	// Ein Array f&uuml;r die Verzeichnisse
	$files = array();	// Ein Array f&uuml;r Dateien
	$prevdirs = array();	// Ein Array für darüber liegende Verzeichnisse

	/**********************************************************
	***********       Verzeichnisse auslesen      *************
	**********************************************************/

	$dir = dir($pathname);
	while ($entry = $dir->read()) {	
		if (	$entry == "." ||
			$entry == ".." || 
			$entry == "banner" ||
			$entry == "pic" ||
			$entry == ".svn") 
			continue;
		if ($pathname != ".")
			$entry = $pathname . "/" . $entry;
		if (is_dir($entry)) 
		{
			$dirs[] = $entry;
		} else
		{
			$add=true;
			if (ereg (".png$", $entry))
				$add=false;
			switch(basename($entry)) {
			case "index.html":
			case "greeting.html":
			case "title.txt":
			case "meta.html":
				$add=false;
			}
			if ($add) $files[] = $entry;
		}
	}
	$dir->close();
	
	if ($pathname != ".")	// keine vorige Ebene vorhanden
	{
		$pdir = dir(dirname($pathname));
		while ($entry = $pdir->read()) {
			if (	$entry == "." ||
				$entry == ".." || 
				$entry == "banner" ||
				$entry == "pic" ||
				$entry == ".svn") 
				continue;
			if (is_dir (dirname($pathname) . "/" . $entry))
				$prevdirs[] = $entry;
		}
		$pdir->close();
	} else {
		$prevdirs[] = $pathname;
	}

	sort($prevdirs);
	sort($dirs);
	sort($files);
	
	/**********************************************************
	***********        Home/Zurück ausgeben       *************
	**********************************************************/

	
	if ($pathname == ".")	// erste Ebene
	{
		echo '		<li class="';
		if ($fileselected == "news.html") echo "file_folder";
		else if ($fileselected == "images.html") echo "file_folder";
		else if ($fileselected == "references.html") echo "file_folder";
		else echo "file_selected";
		echo '"> <a href="/">Home</a></li>', "\n";
	}
	else if (dirname($pathname) == ".")	// zweite Ebene
	{
		echo '		<li class="file_folder"> <a href="/',
			'">Home</a></li>', "\n";

	} else {	// andere Ebene
		echo '		<li class="file_folder"> <a href="/' .
		dirname($pathname) . '">Zur&uuml;ck</a></li>', "\n";
	}
	
	/**********************************************************
	***********       Menüstruktur ausgeben       *************
	**********************************************************/

	foreach ($prevdirs as $prevdir)
	{
		if (dirname($pathname) == ".") $prevdir = $prevdir;
		else $prevdir = dirname($pathname) . "/" . $prevdir;
		if (strcmp($prevdir, $pathname))
		{
			echo '		<li class="file_folder"> <a href="/' , 
			$prevdir , '">' , 
			format_dir ($prevdir) , '</a></li>' , "\n";
		} else {
			if ($prevdir !=".")
			{
				echo '		<li class="';
				if ($fileselected == "index.html") echo "file_selected";
				else echo "file_folder";	// gerade nicht ausgewählt
				echo '"> <a href="/' , 
				$prevdir , '">' , 
				format_dir ($prevdir) , '</a></li>' , "\n";
			}

			if ($pathname != ".") echo '		<li class="mysmaller"><ul>' . "\n";
			foreach ($dirs as $dir) {
				echo '			<li class="';
				if (($fileselected == "images.html" && $filename == $dir) ||
					($fileselected == "news.html" && $filename == $dir) ||
					($fileselected == "references.html" && $filename == $dir))
					echo "file_selected_smaller";
				else echo "file_folder_smaller";
				echo '"> <a href="/' , 
				$dir , '">' , 
				format_dir ($dir) , '</a></li>' , "\n";
			}
			if ($pathname == ".") continue;	// in root keine Dateien anzeigen
			
			foreach ($files as $file) show_file_name ($file, $fileselected);
			echo '		</ul></li>' . "\n";
		}
	}
} //ende function generate_menu

/**Generiert aus dem Ordner banner eine Bildervorschau
 * die rechts unten angezeigt wird. Eine gleichnamige .txt
 * Datei kann verwendet werden um einen Link für dieses
 * Bild zu bestimmen*/
function generate_banner()
{
	$dir = dir("banner");
	while ($entry = $dir->read()) {	
		if (	$entry == '.'||
			$entry == '..' ||
			$entry == '.svn' ||
			ereg (".txt$", $entry))
			continue;
		echo '		<a href="';
		readfile ("banner/$entry.txt");
		echo '		">';
		echo '<img class="pic_banner" src="/banner/',$entry,'" alt="',$entry,'" />';
		echo "</a>", "\n";
	}
	$dir->close();
}

/**Diese Funktion generiert Nachrichten. Damit erkannt wird,
 * dass es sich um einen Ordner mit Nachrichten handelt die
 * Datei news.html anlegen. Alle weiteren html Dateien werden
 * als Nachrichten betrachtet.
 * Sie werden je nach Erstellungsdatum sortiert*/
function generate_news($folder)
{
	echo "<table>";
	$files = array();
	$dir = dir("$folder");
	while ($entry = $dir->read()) {	
		if (	$entry == '.'||
			$entry == '..' ||
			$entry == "news.html")
			continue;
		$fullpath="$folder/$entry";
		$mtime = filemtime ($fullpath);
		$f = array($fullpath => $mtime);
		$files = array_merge ($files, $f);
	}
	$dir->close();

	arsort ($files, SORT_NUMERIC);
	
	foreach ($files as $entry => $time)
	{
		echo "\t".'<tr class="newsheading">' . "\n";
		echo "\t\t".'<td width="80%"><h2>' . 
			ereg_replace(".html","",basename($entry)) . "</h2></td>\n\t\t".
			'<td width="20%">' .	date ("F d Y H:i:s", $time) .
			"</td>\n\t\t</tr>\n";
		echo "\t<tr>\n\t\t".'<td class="newsfooting" colspan=2>'."\n";
		readfile ("$entry");
		echo "\t\t</td>\n\t</tr>\n";
	}
	echo "</table>";
}

/**Generiert Bilder in einem Ordner. Damit erkannt
 * wird, dass dieser Ordner eine Bildervorschau
 * erstellen soll, muss eine Datei images.html
 * angelegt werden.*/
function generate_images($folder)
{
	$files = array();
	$dir = dir("$folder");
	while ($entry = $dir->read()) {	
		if (	$entry == '.'||
			$entry == '..' ||
			$entry == "images.html")
			continue;
		$fullpath="$folder/$entry";
		$mtime = filemtime ($fullpath);
		$fullpath="/$fullpath";
		$f = array($fullpath => $mtime);
		$files = array_merge ($files, $f);
	}
	$dir->close();

	arsort ($files, SORT_NUMERIC);
	
	$num = -1;
	echo "\t\t<table>";
	echo "\t\t" . '<tr class="pic_tr">' . "\n";
	foreach ($files as $entry => $time)
	{
		if ($num++ == 2) {
			echo "\t\t</tr>\n\t\t" . '<tr class="pic_tr">' . "\n";
			$num = 0;
		}
		echo "\t\t\t<td><h2>" .
			ereg_replace(".jpg","",basename($entry)) . "</h2>\n" .
			"\t\t\t<p class=pic_date>" . date ("F d Y H:i:s", $time) . "</p>\n";
		echo "\t\t\t".'<a href="' . "/pic/$entry" . '"><img alt="' . $entry . 
			'" src="' . $entry . '"></a>';
		echo "</td>\n";
	}
	echo "</tr>";
	echo "</table>";
}

/**Generiert Referenzen in einen Ordner. Dabei
 * werden Bilder dargestellt und rechts daneben
 * ist ein Begleittext.*/
function generate_references($folder)
{
	$files = array();
	$dir = dir("$folder");
	while ($entry = $dir->read()) {	
		if (	$entry == '.'||
			$entry == '..' ||
			ereg (".html$", $entry) ||
			$entry == "references.html")
			continue;
		$fullpath="$folder/$entry";
		$mtime = filemtime ($fullpath);
		$f = array($fullpath => $mtime);
		$files = array_merge ($files, $f);
	}
	$dir->close();

	arsort ($files, SORT_NUMERIC);
	
	echo "\t\t<table cellspacing=0 cellpadding=0 border=2>";
	foreach ($files as $entry => $time)
	{
		echo "\t\t".'<tr class="pic_ref">' . "\n";
		echo "\t\t\t<td>"."\n";
		echo "\t\t\t".'<img src="/'.$entry.'" alt="'.$entry.'">'."\n";
		echo "\t\t\t</td>\n";
		echo "\t\t\t<td>\n";
		readfile ("$entry.html");
		echo "\t\t\t</td>\n";
		echo "\t\t</tr>\n";
	}
	echo "\t\t</table>\n";
}
?>
