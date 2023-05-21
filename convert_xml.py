from typing import List, TypedDict
import xml.etree.ElementTree as ET
import csv


class Word(TypedDict):
    word: str
    normalized: str
    glossary: List[str]


def main():
    tree = ET.parse("JMdict_e")

    i = 0

    words: List[Word] = []

    for child in tree.iter("entry"):
        if child.find("k_ele") is None:
            continue

        if child.find("k_ele").find("keb") is None:
            continue

        if should_skip(child):
            continue

        word = child.find("k_ele").find("keb").text
        without_kanji = child.find("r_ele").find("reb").text
        glossary = [x.text for x in child.iter("gloss")]
        words.append({
            'word': word,
            'normalized': without_kanji,
            'glossary': glossary
        })
        # print(word, without_kanji, glossary)
        # if i < 10:
        #     i += 1
        # else:
        #     break

    with open("words.csv", mode="w+") as csvfile:
        writer = csv.writer(csvfile, delimiter=',', quotechar='\'')
        for word in words:
            writer.writerow(
                [word["word"], word["normalized"], ",".join(word["glossary"])])


ignored_fields = ["field", "misc", "pos", "re_inf"]


def should_skip(child: ET.Element) -> bool:
    for field in ignored_fields:
        if child.find(field) is not None:
            print(f"Found field {field} in child, skipping")
            return True

    return False


if __name__ == '__main__':
    main()
