import csv
import gzip
import urllib.request
import xml.etree.ElementTree as ET


def main():
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

                for child in tree.iter("entry"):
                    k_ele = child.find("k_ele")

                    if k_ele is None:
                        continue

                    if k_ele.find("keb") is None:
                        continue

                    if should_skip(child):
                        continue

                    word = k_ele.find("keb").text
                    normalized = child.find("r_ele").find("reb").text
                    glossary = [x.text for x in child.iter("gloss")]

                    writer.writerow([word, normalized, "|".join(glossary)])


ignored_fields = ["field", "misc"]


# Ignore every word not in the top 24 000 most used words
def should_skip(child: ET.Element) -> bool:
    ke_pris = child.find("k_ele").findall("ke_pri")

    if len(ke_pris) == 0:
        return True

    for kePri in ke_pris:
        if kePri.text in ["news1", "news2"]:
            return False

    return True


if __name__ == '__main__':
    main()
