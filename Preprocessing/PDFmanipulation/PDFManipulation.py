__author__ = 'Dean'

import glob
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfinterp import PDFPageInterpreter, PDFResourceManager
from cStringIO import StringIO
from pdfminer.layout import LAParams
from pdfminer.converter import TextConverter

def main():
    resourcePath = "E:\TestData\*.pdf"
    filenamearray = getFilepaths(resourcePath)
   # Test print
    print (convertPDFToText(filenamearray[9]))











def convertPDFToText(path):
    resourceManager = PDFResourceManager()
    retstr = StringIO()
    codec = 'utf-8'
    laparams = LAParams()
    device = TextConverter(resourceManager, retstr, codec=codec, laparams=laparams)
    fp = file(path, 'rb')
    interpreter = PDFPageInterpreter(resourceManager, device)
    password = ""
    maxPages = 0
    caching = True
    pageNumbers = set()
    for page in PDFPage.get_pages(fp, pageNumbers, maxpages=maxPages, password=password,caching=caching, check_extractable=True):
        interpreter.process_page(page)
    fp.close()
    device.close()
    str = retstr.getvalue()
    retstr.close()
    return str


def getFilepaths(sysPath):
    fileArray = glob.glob(sysPath)
#   for f in fileArray:
#      print(f)
    return fileArray

main()
