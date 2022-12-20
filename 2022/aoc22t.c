#include <stdio.h>

const int example[] = {1, 2, -3, 3, -2, 0, 4};
enum { N = sizeof example / sizeof example[0] };

struct Node {
    struct Node* prev;
    struct Node* next;
    long long payload;
};

struct Node circle[N];
struct Node* zero;

void buildChain(long long decryptionKey) {
    for (int i = 0; i < N; ++i) {
        if (example[i] == 0) {
            zero = &circle[i];
        }
        circle[i].prev = &circle[(i - 1 + N) % N];
        circle[i].next = &circle[(i + 1) % N];
        circle[i].payload = example[i] * decryptionKey;
    }
}

void mixChain() {
    for (int i = 0; i < N; ++i) {
        struct Node* p = circle[i].prev;
        struct Node* q = circle[i].next;
        // unlink from chain
        p->next = q;
        q->prev = p;
        // find new neighbours
        int delta = circle[i].payload % (N - 1);
        for (; delta > 0; --delta) {
            p = q;
            q = q->next;
        }
        for (; delta < 0; ++delta) {
            q = p;
            p = p->prev;
        }
        // relink to chain
        p->next = &circle[i];
        circle[i].prev = p;
        circle[i].next = q;
        q->prev = &circle[i];
    }
}

void printGroveCoordinates(const char* part) {
    struct Node* p = zero;
    long long sum = 0;
    for (int t = 0; t < 3; ++t) {
        for (int i = 0; i < 1000; ++i) {
            p = p->next;
        }
        sum += p->payload;
    }
    printf("%s: %lld\n", part, sum);
}

int main() {
    buildChain(1LL);
    mixChain();
    printGroveCoordinates("Part 1");

    buildChain(811589153LL);
    for (int round = 0; round < 10; ++round) {
        mixChain();
    }
    printGroveCoordinates("Part 2");
}
