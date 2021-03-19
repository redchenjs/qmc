/*
 * qmc.cpp
 *
 *  Created on: 2021-03-17 18:45
 *      Author: Jack Chen <redchenjs@live.com>
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>

#define MAX_VAR_N 5
#define MAX_ITM_N 32

const char out_tbl[MAX_VAR_N * 2][3] = {"A", "B", "C", "D", "E", "A'", "B'", "C'", "D'", "E'"};
const char bit_tbl[MAX_ITM_N] = {0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5};

// Group - Term - Data
typedef struct {
    uint32_t term_mask;
    char term_num[MAX_VAR_N + 1][MAX_ITM_N];
    char term_data[MAX_VAR_N + 1][MAX_ITM_N][MAX_ITM_N];
    char comb_num[MAX_VAR_N + 1];
    char comb_data[MAX_VAR_N + 1][MAX_ITM_N][MAX_VAR_N + 1];
} term_data_t;

term_data_t out = {0};
char input[MAX_ITM_N] = {0};

int sort_cmpfunc(const void *a, const void *b)
{
   return (*(char *)a - *(char *)b);
}

void load_data(int n, int m, int d)
{
    for (int g = 0; g <= n; g++) {
        for (int i = 0; i < m + d; i++) {
            out.term_mask |= (i < m) ? 0x01 << input[i] : 0x00;

            if (bit_tbl[input[i]] == g) {
                int term = out.comb_num[g];
                int temp = input[i];

                for (int k = 0; k < n; k++) {
                    char data = (temp & (0x01 << (n - 1))) ? '1' : '0';

                    out.comb_data[g][term][k] = data;

                    temp <<= 1;
                }

                out.comb_num[g]++;
                out.comb_data[g][term][n] = 'm';

                out.term_num[g][term]++;
                out.term_data[g][term][0] = input[i];
            }
        }
    }
}

void combine_terms(int n)
{
    term_data_t tmp = {0};

    for (int g = 0; g < n; g++) {
        for (int t0 = 0; t0 < out.comb_num[g]; t0++) {
            for (int t1 = 0; t1 < out.comb_num[g + 1]; t1++) {
                int diff = 0;
                for (int k = 0; k < n; k++) {
                    if (out.comb_data[g][t0][k] != out.comb_data[g + 1][t1][k]) {
                        diff++;
                    }
                }

                if (diff == 1) {
                    for (int k = 0; k < n; k++) {
                        if (out.comb_data[g][t0][k] != out.comb_data[g + 1][t1][k]) {
                            int comb_num = tmp.comb_num[g];
                            int term_num = tmp.term_num[g][comb_num];

                            for (int i = 0; i < out.term_num[g][t0]; i++) {
                                tmp.term_data[g][comb_num][term_num++] = out.term_data[g][t0][i];
                                tmp.term_num[g][comb_num]++;
                            }

                            for (int i = 0; i < out.term_num[g + 1][t1]; i++) {
                                tmp.term_data[g][comb_num][term_num++] = out.term_data[g + 1][t1][i];
                                tmp.term_num[g][comb_num]++;
                            }

                            memcpy(tmp.comb_data[g][comb_num], out.comb_data[g + 1][t1], sizeof(out.comb_data[g][t0]));
                            tmp.comb_data[g][comb_num][n] = 'm';
                            tmp.comb_data[g][comb_num++][k] = '-';
                            tmp.comb_num[g]++;

                            out.comb_data[g][t0][n] = '*';
                            out.comb_data[g + 1][t1][n] = '*';
                        }
                    }
                }
            }
        }
    }

    for (int g = 0; g <= n; g++) {
        for (int t = 0; t < out.comb_num[g]; t++) {
            if (out.comb_data[g][t][n] == '*') {
                continue;
            }

            int comb_num = tmp.comb_num[g];
            int term_num = tmp.term_num[g][comb_num];

            memcpy(tmp.term_data[g][comb_num], out.term_data[g][t], sizeof(out.term_data[g][t]));
            tmp.term_num[g][comb_num] = out.term_num[g][t];

            memcpy(tmp.comb_data[g][comb_num], out.comb_data[g][t], sizeof(out.comb_data[g][t]));
            tmp.comb_num[g]++;
        }
    }

    tmp.term_mask = out.term_mask;

    memcpy(&out, &tmp, sizeof(term_data_t));
}

void clear_terms(int n)
{
    for (int g = 0; g <= n; g++) {
        int diff = 0;

        for (int t0 = 0; t0 < out.comb_num[g] - 1; t0++) {
            for (int t1 = t0 + 1; t1 < out.comb_num[g]; t1++) {
                if (!memcmp(out.comb_data[g][t0], out.comb_data[g][t1], n)) {
                    for (int k = 0; k < out.term_num[g][t1]; k++) {
                        out.term_data[g][t0][out.term_num[g][t0]++] = out.term_data[g][t1][k];
                    }

                    qsort(out.term_data[g][t0], out.term_num[g][t0], 1, sort_cmpfunc);

                    char *ret = std::unique(out.term_data[g][t0], out.term_data[g][t0] + out.term_num[g][t0]);
                    out.term_num[g][t0] = ret - out.term_data[g][t0];

                    out.comb_data[g][t1][n] = '*';

                    diff++;
                }
            }
        }

        out.comb_num[g] -= diff;
    }
}

void find_prime(int n, int m, int d)
{
    static int loop = 0;
    int count = 0, max_count = 0;
    char group = 0, comb_num = 0;

    if (!out.term_mask) {
        return;
    }

    printf("Loop %d: %08x ", loop++, out.term_mask);

    // find the term that contains a single minterm
    for (int i = 0; i < m; i++) {
        count = 0;

        for (int g = 0; g <= n; g++) {
            for (int num = 0, t = 0; num < out.comb_num[g]; num++, t++) {
                if (out.comb_data[g][t][n] == '*') {
                    num--;
                    continue;
                }

                for (int k = 0; k < out.term_num[g][t]; k++) {
                    if (input[i] == out.term_data[g][t][k]) {
                        count++;

                        group = g;
                        comb_num = t;
                    }
                }
            }
        }

        if (count == 1) {
            out.comb_data[group][comb_num][n] = 'x';

            for (int k0 = 0; k0 < out.term_num[group][comb_num]; k0++) {
                int found = out.term_data[group][comb_num][k0];

                for (int g = 0; g <= n; g++) {
                    for (int num = 0, t = 0; num < out.comb_num[g]; num++, t++) {
                        if (out.comb_data[g][t][n] == '*') {
                            num--;
                            continue;
                        }

                        for (int k1 = 0; k1 < out.term_num[g][t]; k1++) {
                            if (found == out.term_data[g][t][k1]) {
                                out.term_data[g][t][k1] = -1;
                            }
                        }
                    }
                }

                out.term_mask &= ~(0x01 << found);
            }
        }
    }

    // find the term that contains most minterms
    max_count = 0, group = 0, comb_num = 0;
    for (int g = 0; g <= n; g++) {
        for (int num = 0, t = 0; num < out.comb_num[g]; num++, t++) {
            if (out.comb_data[g][t][n] == '*') {
                num--;
                continue;
            }

            if (out.comb_data[g][t][n] == 'x') {
                continue;
            }

            count = 0;
            for (int k = 0; k < out.term_num[g][t]; k++) {
                for (int i = 0; i < m; i++) {
                    if (input[i] == out.term_data[g][t][k]) {
                        count++;

                        if (max_count < count) {
                            max_count = count;

                            group = g;
                            comb_num = t;
                        }
                    }
                }
            }
        }
    }

    if (max_count != 0) {
        out.comb_data[group][comb_num][n] = 'x';

        for (int k0 = 0; k0 < out.term_num[group][comb_num]; k0++) {
            int found = out.term_data[group][comb_num][k0];

            for (int g = 0; g <= n; g++) {
                for (int num = 0, t = 0; num < out.comb_num[g]; num++, t++) {
                    if (out.comb_data[g][t][n] == '*') {
                        num--;
                        continue;
                    }

                    for (int k1 = 0; k1 < out.term_num[g][t]; k1++) {
                        if (found == out.term_data[g][t][k1]) {
                            out.term_data[g][t][k1] = -1;
                        }
                    }
                }
            }

            out.term_mask &= ~(0x01 << found);
        }
    }

    printf("=> %08x\n", out.term_mask);
}

void check_terms(int n, int m, int d)
{
    for (int g = 0; g <= n; g++) {
        int comb_num = out.comb_num[g];

        for (int num = 0, t = 0; num < comb_num; num++, t++) {
            if (out.comb_data[g][t][n] == '*') {
                num--;
                continue;
            }

            int mterm = 0, dterm = 0;
            for (int k = 0; k < out.term_num[g][t]; k++) {
                for (int i = 0; i < m; i++) {
                    if (input[i] == out.term_data[g][t][k]) {
                        mterm++;
                    }
                }

                for (int i = m; i < m + d; i++) {
                    if (input[i] == out.term_data[g][t][k]) {
                        dterm++;
                    }
                }
            }

            if (mterm == 0 && dterm != 0) {
                out.comb_num[g]--;
                out.comb_data[g][t][n] = '*';
            }
        }
    }
}

void print_terms(int n)
{
    for (int g = 0; g <= n; g++) {
        printf("Group %d: ", g);

        for (int num = 0, t = 0; num < out.comb_num[g]; num++, t++) {
            if (out.comb_data[g][t][n] == '*') {
                num--;
                continue;
            }

            for (int k = 0; k < out.term_num[g][t]; k++) {
                printf("%d ", out.term_data[g][t][k]);
            }

            printf("  ");
        }

        printf("\n");
    }
}

void print_data(int n)
{
    for (int g = 0; g <= n; g++) {
        printf("Group %d: ", g);

        for (int num = 0, t = 0; num < out.comb_num[g]; num++, t++) {
            if (out.comb_data[g][t][n] == '*') {
                num--;
                continue;
            }

            for (int k = 0; k <= n; k++) {
                printf("%c ", out.comb_data[g][t][k]);
            }

            printf("  ");
        }

        printf("\n");
    }
}

void show_output(int n)
{
    char empty = 1;

    for (int g = 0; g <= n; g++) {
        for (int num = 0, t = 0; num < out.comb_num[g]; num++, t++) {
            if (out.comb_data[g][t][n] == '*') {
                num--;
                continue;
            }

            if (out.comb_data[g][t][n] == 'x') {
                if (!empty) {
                    printf(" + ");
                } else {
                    printf("=> F = ");
                }

                for (int k = 0; k < n; k++) {
                    if (!out.comb_data[g][t][k]) {
                        break;
                    }

                    if (out.comb_data[g][t][k] != '-') {
                        empty = 0;
                        printf("%s", out.comb_data[g][t][k] == '1' ? out_tbl[k] : out_tbl[k + 5]);
                    }
                }
            }
        }
    }

    if (!empty) {
        printf("\n");
    } else if (n == 1) {
        printf("1\n");
    }
}

int main(void)
{
    term_data_t test = {0};
    int n = 0, m = 0, d = 0;

    printf("-------------------------Input--------------------------\n");
    printf("=> N M D M{...} D{...}\n");
    printf("=> N: Number of variables (MAX = %d)\n", MAX_VAR_N);
    printf("=> M: Number of minterms (M + D <= %d)\n", 0x01 << MAX_VAR_N);
    printf("=> D: Number of don't-care terms (M + D <= %d)\n", 0x01 << MAX_VAR_N);
    printf("=> M{...}: Minterms\n");
    printf("=> D{...}: Don't-care terms\n");

    printf("=> ");

    scanf("%d %d %d", &n, &m, &d);

    if (n < 1 || n > MAX_VAR_N || (m + d) > (0x01 << MAX_VAR_N)) {
        printf("-------------------------Error--------------------------\n");
        return -1;
    }

    for (int i = 0; i < m + d; i++) {
        scanf("%d", &input[i]);
    }

    printf("------------------Step 1: Load Data---------------------\n");

    load_data(n, m, d);

    print_data(n);

    printf("------------------Step 2: Combine Terms-----------------\n");

    do {
        memcpy(&test, &out, sizeof(term_data_t));

        combine_terms(n);
    } while (memcmp(&test, &out, sizeof(term_data_t)));

    clear_terms(n);

    print_data(n);

    printf("------------------Step 3: Find Prime--------------------\n");

    print_terms(n);

    do {
        memcpy(&test, &out, sizeof(term_data_t));

        find_prime(n, m, d);
    } while (memcmp(&test, &out, sizeof(term_data_t)));

    printf("------------------Step 4: Check Terms-------------------\n");

    check_terms(n, m, d);

    print_data(n);

    printf("-------------------------Output-------------------------\n");

    show_output(n);

    return 0;
}
