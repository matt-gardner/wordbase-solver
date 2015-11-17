#!/usr/bin/env python
# -*- encoding: utf-8 -*-


def main(filename, outfile):
    words = set()
    for line in open(filename):
        word = line.split()[0]
        words.add(word)
    words = map(remove_diacritics, words)
    words = list(set(words))
    words.sort()
    out = open(outfile, 'w')
    for word in words:
        out.write(word.encode('utf-8'))
        out.write('\n')
    out.close


def remove_diacritics(word):
    word = unicode(word, 'utf-8')
    word = word.replace(u"ñ", u"_ENYE_")
    import unicodedata
    nkfd_form = unicodedata.normalize('NFKD', word)
    word = u"".join([c for c in nkfd_form if ord(c) < 128])
    word = word.replace(u"_ENYE_", u"ñ")
    return word


if __name__ == '__main__':
    main('/home/mg1/clone/wordbase/tmp/es.txt',
         '/home/mg1/clone/wordbase/tmp/spanish_dictionary.txt')

# vim: et sw=4 sts=4
