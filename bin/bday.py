#!/usr/bin/env python
# Calcuate business day offsets

import os
import sys

from datetime import datetime
from pandas.tseries.holiday import USFederalHolidayCalendar
from pandas.tseries.offsets import CustomBusinessDay

def main():
    if len(sys.argv)-1 != 2:
        print('Usage: {:s} date offset'.format(os.path.basename(sys.argv[0])))
        sys.exit(1)
    date, offset = (sys.argv[1:3])
    bday_us = CustomBusinessDay(calendar=USFederalHolidayCalendar())
    print((datetime.strptime(date, '%Y-%m-%d') + int(offset) * bday_us).date())

if __name__ == '__main__':
    main()
