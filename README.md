Surveillance_Capitalism_001 (json export) and Surveillance_Capitalism_002 (kml export). These routines use rdata.table to pull lat,lon and activity state (json only)  from the json or kml exports of Google Location from https://takeout.google.com . You must have a location store of data. Adjust setwd() as needed. Microsoft Open R 3.53 (Intel MKL) with data.table 1.12.2 Also: setMKLthreads(4) and setDTthreads(0) . For a blog piece and code output examples see https://mathandscienceprogramming.blogspot.com/2019/09/google-geo-location-data-code-and.html .
