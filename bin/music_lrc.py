#!/usr/bin/env python3

# Else go for https://www.lyricsify.com

import os
import requests
from mutagen.easyid3 import EasyID3
from mutagen.mp3 import MP3

def get_metadata(mp3_path):
    try:
        audio = MP3(mp3_path, ID3=EasyID3)
        artist = audio['artist'][0] if 'artist' in audio else None
        title = audio['title'][0] if 'title' in audio else None
        return artist, title
    except Exception:
        return None, None

def fetch_lrc(artist, title):
    try:
        res = requests.get(f"https://lrclib.net/api/search", params={'artist_name': artist, 'track_name': title}, timeout=10)
        res.raise_for_status()
        data = res.json()
        if not data:
            return None

        first = data[0]
        lrc_res = requests.get('https://lrclib.net/api/get/{}'.format(first['id']), params={}, timeout=10)
        lrc_res.raise_for_status()
        data_res = lrc_res.json()
        if not data_res or 'syncedLyrics' not in data_res:
            return None
        syncedLyrics = data_res['syncedLyrics']
        return syncedLyrics
    except Exception:
        return None

def scan_and_download_lrc(directory):
    for root, _, files in os.walk(directory):
        for file in files:
            if file.lower().endswith(".mp3"):
                mp3_path = os.path.join(root, file)
                lrc_path = os.path.splitext(mp3_path)[0] + ".lrc"

                if os.path.exists(lrc_path):
                    continue

                artist, title = get_metadata(mp3_path)
                if not artist or not title:
                    print(f"Skipping {file}: missing metadata")
                    continue

                lrc_content = fetch_lrc(artist, title)
                if not lrc_content:
                    print(f"LRC not found: {artist} - {title}")
                    continue

                with open(lrc_path, "w", encoding="utf-8") as f:
                    f.write(lrc_content)
                print(f"LRC saved for: {file}")

if __name__ == "__main__":
    import sys
    target_dir = sys.argv[1] if len(sys.argv) > 1 else os.getcwd()
    scan_and_download_lrc(target_dir)

