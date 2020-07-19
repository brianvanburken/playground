from typing import TextIO
from  io  import  StringIO
import  time_series
    
def  process_file(reader: TextIO) -> int:

    """Read and process reader, which must start with a time_series header. 
    Return the largest value after the header.  There may be multiple pieces 
    of data on each line. 
    >>> infile = StringIO('Example \\ n 20. 3. \\ n 100. 17. 15. \\ n') 
    >>> process_file(infile) 
    100 
    """ 
   line = time_series.skip_header(reader).strip()
    # The largest value so far is the largest on this first line of data. 
   largest = find_largest(line)
    # To read the rest of the date at one, read() should be used, it keeps on reading until encounters EOF .
    text = reader.read()
    print(text)
