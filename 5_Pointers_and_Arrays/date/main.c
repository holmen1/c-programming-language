#include <stdio.h>

static char daytab[2][13] = {
    {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}};

int day_of_year(int year, int month, int day)
{
    int leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;
    char *p = *(daytab + leap);

    while (--month > 0)
        day += *(++p);
    return day;
}

void month_day(int year, int yearday, int *pmonth, int *pday)
{
    int leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;
    char *p = *(daytab + leap);
    
    *pmonth = 0;

    while (1) {
        *pmonth += 1;
        p++;
        
        if (yearday > *p) { /* *p now holds the number of days in the current month (*pmonth) */
            yearday -= *p;
        } else {
            break;
        }
    }
    *pday = yearday;
}

int main()
{
    int month, day;
    day = day_of_year(2025, 3, 1);
    printf("day_of_year(2025, 3, 1) = %d\n", day);

    month_day(2025, 91, &month, &day);
    printf("month_day(2025, 91) = %d, %d\n", month, day);
    
    return 0;
}

/*
day_of_year(2025, 3, 1) = 60
month_day(2025, 91) = 4, 1
*/
