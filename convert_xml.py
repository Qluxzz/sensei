import csv
import gzip
from typing import List
import urllib.request
import xml.etree.ElementTree as ET


def main():
    """Downloads and convert a japanese to english dictionary from XML to csv
    """
    # https://www.edrdg.org/wiki/index.php/JMdict-EDICT_Dictionary_Project

    with urllib.request.urlopen("http://ftp.edrdg.org/pub/Nihongo/JMdict_e.gz") as gziped:
        with gzip.open(gziped) as file:
            with open("words.csv", mode="w+") as csv_file:
                writer = csv.writer(
                    csv_file,
                    delimiter=',',
                    quotechar='\'',
                    escapechar='\\',
                    quoting=csv.QUOTE_ALL
                )

                tree = ET.parse(file)

                for entry in tree.iter("entry"):
                    if data := parse_entry(entry):
                        writer.writerow(data)


def parse_entry(entry: ET.Element) -> None | List[str]:
    if should_skip(entry):
        return None

    word = entry.find("k_ele").find("keb").text
    kana = entry.find("r_ele").find("reb").text
    glossary = [x.text for x in entry.iter("gloss")]

    return [word, kana, "|".join(glossary)]


def should_skip(child: ET.Element) -> bool:
    """ Skips if no kana representation or not in the top 24 000 most used words"""

    """
        The kanji element, or in its absence, the reading element, is 
        the defining component of each entry.
        The overwhelming majority of entries will have a single kanji
        element associated with a word in Japanese. Where there are 
        multiple kanji elements within an entry, they will be orthographical
        variants of the same word, either using variations in okurigana, or
        alternative and equivalent kanji. Common "mis-spellings" may be 
        included, provided they are associated with appropriate information
        fields. Synonyms are not included; they may be indicated in the
        cross-reference field associated with the sense element.
    """
    k_ele = child.find("k_ele")

    if k_ele is None:
        return True

    """
        This element will contain a word or short phrase in Japanese 
        which is written using at least one non-kana character (usually kanji,
        but can be other characters). The valid characters are
        kanji, kana, related characters such as chouon and kurikaeshi, and
        in exceptional cases, letters from other alphabets.
    """
    if k_ele.find("keb") is None:
        return True

    """
        This and the equivalent re_pri field are provided to record
        information about the relative priority of the entry, and consist
        of codes indicating the word appears in various references which
        can be taken as an indication of the frequency with which the word
        is used. This field is intended for use either by applications which 
        want to concentrate on entries of  a particular priority, or to 
        generate subset files.
    """
    ke_pris = child.find("k_ele").findall("ke_pri")

    if len(ke_pris) == 0:
        return True

    for kePri in ke_pris:
        """
            news1/2: appears in the "wordfreq" file compiled by Alexandre Girardi
            from the Mainichi Shimbun. (See the Monash ftp archive for a copy.)
            Words in the first 12,000 in that file are marked "news1" and words 
            in the second 12,000 are marked "news2"
        """
        if kePri.text in ["news1", "news2"]:
            return False

    return True


if __name__ == '__main__':
    main()
