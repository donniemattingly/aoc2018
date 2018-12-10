//
// Created by Donnie Mattingly on 2018-12-09.
//

#include <iostream>
#include <vector>

using namespace std;

struct Node {
    long data;
    struct Node *next;
    struct Node *prev;
};


void printNNodes(long n, Node *nodeptr){
    Node* cur = nodeptr;
    for(long i=0;i<n;i++){
        printf(" %d", cur->data);
        cur = cur->next;
    }
}

class MarbleCircle {
public:
    Node *current;

    long removeNode(Node *nodePtr) {
        Node node = *nodePtr;

        long score = node.data;
        Node prev = *node.prev;
        Node next = *node.next;

        prev.next = &next;
        next.prev = &prev;

        return score;
    }

    long placeMarble(long data) {
//        printf("data was: %d\n", data);
        if (current == nullptr) {
            Node* newNode = new Node{};
            newNode->data = data;
            newNode->prev = newNode;
            newNode->next = newNode;

            current = newNode;

            return 0;
        }

        if (data % 23 == 0) {
            Node removeNode = *current->prev->prev->prev->prev->prev->prev->prev;
            long newScore = data + removeNode.data;

            current = removeNode.next;
            current->prev = removeNode.prev;
            removeNode.prev->next = current;

//            printNNodes(25, current);
//            printf("Nice play, scored: %d (%d for their marble and %d for removed marble)\n", newScore, score, removed);
            return newScore;
        } else {

            Node* newPrev = current->next;
            Node* newNext = newPrev->next;

            Node* newNode = new Node{data, newNext, newPrev};

            newPrev->next = newNode;
            newNext->prev = newNode;


            current = newNode;

            return 0;
        }
    }
};

const long testPlayersCount = 9;
const long testLastMarbleValue = 25;

long playGame(long numPlayers, long lastMarbleNum) {
    auto *c = new MarbleCircle();
    vector<long> scores((unsigned long) numPlayers);


    for (long i = 0; i < lastMarbleNum + 1; i++) {
        long moveScore = c->placeMarble(i);
        long oldScore = scores[(i-1) % numPlayers];
        long newScore = moveScore + oldScore;

        if(moveScore != 0){
//            printf("\nmove scored: %d which gives new score of: %d\n", moveScore, newScore);
        }
        scores[(i-1) % numPlayers] = newScore;
    }

//    for (auto i = scores.begin(); i != scores.end(); ++i)
//        std::cout << *i << ' ';

    auto max = max_element(scores.begin(), scores.end());

    return *max;
}


void testInput() {
    long testValues[][3] = {
            {9, 25, 32},
            {10, 1618, 8317},
            {13, 7999, 146373},
            {17, 1104, 2764},
            {21, 6111, 54718},
            {30, 5807, 37305},
            {426, 72058, -1},
            {426, 7205800, -1},
    };

    for (auto row : testValues) {
        auto max = playGame(row[0], row[1]);
        std::cout << "Actual: ";
        std::cout << max;
        std::cout << "     Expected: ";
        std::cout << row[2];
        std::cout << "\n";
    }
}

void partOne() {
    auto max = playGame(426, 72058);
    printf("Part One: %ld\n", max);
}

void partTwo() {
    auto max = playGame(426, 7205800);
    printf("Part Two: %ld\n", max);
}

int main() {
    partOne();
    partTwo();
}

