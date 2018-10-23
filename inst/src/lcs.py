
def longest_common_subsequence(first_seq, second_seq):
    """Computes the LCS between two sequences.

    The function below takes as input two sequences, computes the LCS between them,
    and stores it in C[i,j]. C[m,n] will contain indexes of first_seq which contain the LCS.
    Both sequences should have comparable elements. Complexity O(n*m)

    :param first_seq: iterable - first sequence.
    :param second_seq: iterable - second sequence.
    :return: list of indexes of first_seq which are not included on LCS
    """
    m = len(first_seq)
    n = len(second_seq)
    # An (m+1) times (n+1) matrix of empty lists
    C = [[list()] * (n + 1) for _ in range(m + 1)]
    for i in range(1, m+1):
        for j in range(1, n+1):
            if first_seq[i - 1] == second_seq[j - 1]:
                C[i][j] = [i-1, ]
                C[i][j].extend(C[i-1][j-1])
            else:
                C[i][j] = (max(C[i-1][j], C[i][j-1], key=len))
    lcs = C[m][n]
    result = list()
    for i in range(0, m):
        if i not in lcs:
            result.append(i)
    return result
