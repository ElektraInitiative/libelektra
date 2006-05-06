#!/usr/bin/php 
<?
/*
 * KDE Configuration import
 *
 * Yannick Lecaillez
 * <sizon5@gmail.com>
 */

function convert($file, $rootKey)
{
	$currentGroup = "<default>";
	$keyName = NULL;
	$keyValue = NULL;

/*	if ( substr($file, -2) != 'rc' )
		return; */
	
	$fp = fopen($file, "r");
	if ( !is_resource($fp) ) {
		echo "Can't open $file\n";
		return 1;
	}
	$stat = fstat($fp);

	while ( !feof($fp) ) {
		$line = trim(ltrim(fgets($fp)));

		if ( empty($line) )
			continue;

		$groupName = NULL;
		$key = NULL;
		$keyName = NULL;
		$keyVal = NULL;

		if ( ereg("^\[(.*)\]$", $line, $groupName) ) {
			$currentGroup = $groupName[1];
		
			/* Replace "/" -> "\/" if its the first char */
/*			if ( $currentGroup[0] == '/' && $currentGroup[1] != '/' )
				$currentGroup = "\/".substr($currentGroup, 1, -1); */

			/* Replace "//" -> "\/\/" */
			$currentGroup = str_replace("/", "\/", $currentGroup);
			continue;
		}

		if ( ($sep = strpos($line, "=")) == FALSE )
			continue;
		$keyName = str_replace("/", "\/", substr($line, 0, $sep));
		$keyVal = substr($line, $sep+1);
	
		$kName = str_replace("\"", "\\\"", "$rootKey$currentGroup/$keyName");
		$kName = str_replace("$", "\\$", $kName);
		
		if ( $keyVal ) {
//			$keyVal = str_replace("\"", "\\\"", $keyVal);
			$keyVal = str_replace("$", "\\$", $keyVal);
			
			
//			echo "kdb set -u ".$stat["uid"]." -g ".$stat["gid"]." -m ".decoct($stat["mode"] & 0777)." -- \"$kName\" \"".str_replace("\"", "\\\"", $keyVal)."\"\n";
			echo "<key uid=\"".$stat["uid"]."\" gid=\"".$stat["gid"]."\" mode=\"".decoct($stat["mode"] & 0777)."\" name=\"$kName\"><value>".str_replace("\"", "\\\"", $keyVal)."</value></key>\n";
			
			
		} else {
//			echo "kdb set -u ".$stat["uid"]." -g ".$stat["gid"]." -m ".decoct($stat["mode"] & 0777)." -- \"$kName\"\n";
			echo "<key uid=\"".$stat["uid"]."\" gid=\"".$stat["gid"]."\" mode=\"".decoct($stat["mode"] & 0777)."\" name=\"$kName\"/>\n";
		}
	}

	
	fclose($fp);
}


function convertDir($path, $rootKey)
{
	$dir = opendir($path);
	if ( !is_resource($dir) ) {
		echo "Can't open $path\n";
		return 1;
	}
	rewinddir($dir);

	while ( ($fname = readdir($dir)) != FALSE ) {
		if ( $fname == "." || $fname == ".." || $fname[0] == '.' )
			continue;

		if ( is_dir("$path/$fname") )
			convertDir("$path$fname/", "$rootKey$fname/");
		else
			convert("$path$fname", "$rootKey$fname/");
	}
}

function getHomesDir()
{
	$fp = fopen("/etc/passwd", "r");
	if ( !is_resource($fp) ) {
		echo "Can't open /etc/passwd\n";
		exit;
	}

	$homes = array( "user"	=> array(),
			"home"	=> array());
	while ( !feof($fp) ) {
		$line = trim(ltrim(fgets($fp)));
		if ( empty($line) )
			continue;

		$data = explode(":", $line);

		if ( ereg("^/home/", $data[5]) ) {
			$homes["user"][] = $data[0];
			$homes["home"][] = $data[5];
		}
	}
	
	fclose($fp);

	return $homes;
}

function getKdePath($homes)
{
	exec("kde-config --path config", $output, $ret);
	$path = explode(":", $output[0]);

	$kdePath = array();
	$kdePath["system"] = $path[1];

	$home = getenv("HOME");
	
	if ( ereg("^$home(.*)$", $path[0], $ret) ) {
		$kdePath["user"] = array();
		for($i = 0 ; $i<count($homes) ; $i++) {
//			if ( file_exists($homes[$i].$ret[1]) )
				$kdePath["user"][] = $homes[$i].$ret[1];
		}	
	} else {
		echo "Can't get the user part of the path. Quit";
		exit;
	}
	
	return $kdePath;
}

// convert("/home/yl/.kde/share/config/cervisiarc", "user/sw/kde/");

$users = getHomesDir();
$kdeConf = getKdePath($users["home"]);

convertDir($kdeConf["system"], "system/sw/kde/");
for($i = 0 ; $i<count($kdeConf["user"]) ; $i++) {
	convertDir($kdeConf["user"][$i], "user:".$users["user"][$i]."/sw/kde/");
}

/* $infile = $argv[1];
$keyroot = $argv[2];

convert($infile, $keyroot); */


?>
