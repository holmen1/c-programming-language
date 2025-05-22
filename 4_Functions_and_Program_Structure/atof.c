/* Exercise 4-2 handle scientific notation for single integer exponent */
/* atof */
/* convert string to double precision float */
#include <ctype.h>
#include <stdio.h>
#include <math.h>

double atof(char s[])
{
    double val, power;
    int i, sign, esign;

    for (i = 0; isspace(s[i]); i++)
        ;
    sign = s[i] == '-' ? -1 : 1;
    if (s[i] == '+' || s[i] == '-')
        i++;
    for (val = 0.0; isdigit(s[i]); i++)
        val = 10.0 * val + (s[i] - '0');
    if (s[i] == '.')
        i++;
    for (power = 1.0; isdigit(s[i]); i++) {
        val = 10.0 * val + (s[i] - '0');
        power *= 10.0;
    }
    if (s[i] == 'e' || s[i] == 'E') {
        i++;
        esign = s[i] == '-' ? -1 : 1;
        if (s[i] == '+' || s[i] == '-')
            i++;
        val *= pow(10.0, esign * (s[i] - '0'));
    }

    return sign * val / power;
}

int main()
{
    char str[] = "-42.2e-3";
    printf("atof(\"%s\") = %g\n", str, atof(str));
    char str2[] = "42.2e3";
    printf("atof(\"%s\") = %g\n", str2, atof(str2));
    return 0;
}

/*
atof("-42.2e-3") = -0.0422
atof("42.2e3") = 42200
*/
