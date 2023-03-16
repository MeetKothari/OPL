#ifndef TOMBSTONES_H // if not defined, define
#define TOMBSTONES_H

#include <iostream> // neccessary libraries
#include <stdlib.h>

using namespace std;

template <class T>

class Tombstone {
    
    public:
    T *data;             // MyPointer 
    int count;           // for reference
    bool dead;           // for tombstones
    
    Tombstone<T>() {
        dead = false;
        count = 1;
        data = nullptr;
    }
    
    Tombstone<T>(T *pT) {
        dead = false;
        count = 1;
        data = pT;
    }
};

template <class T>

class MyPointer {
    
    public:
    
    MyPointer <T>() { tombstone = new Tombstone<T>(); } // default constructor

    MyPointer <T>(const MyPointer <T> &t) {
        
        if (t.tombstone->dead != true) {
            
            tombstone = t.tombstone;
            tombstone++->count;
        }
        
        else tombstone = new Tombstone<T>();
        
    } // copy constructor

    MyPointer <T>(T *pT) { tombstone = new Tombstone<T>(pT); } // bootstrapping constructor
    ~MyPointer <T>() { --tombstone->count; } // destructor
    
    T &operator*() const {
        
        if (tombstone->dead) {
            cout << "Error: dangling MyPointer !" << endl;
            exit(1);
        }
        
        else if (tombstone->count <= 0) {
            cout << "Error: avoided memory leak!" << endl;
            exit(1);
        }
        
        return *(tombstone->data);
    } // defererencing
    
    T *operator->() const {
        
        if (tombstone->dead) {
            cout << "Error: dangling MyPointer !" << endl;
            exit(1);
        }
        else if (tombstone->count <= 0)
        {
            cout << "Error: avoided memory leak!" << endl;
            exit(1);
        }
        return this->tombstone->data;
    } // field dereferencing
    
    MyPointer <T> &operator=(const MyPointer <T> &t) {
        
        if (tombstone != t.tombstone){
            --tombstone->count;
            tombstone = t.tombstone;
            ++tombstone->count;
        }
        return *this;
    } // assignment

    friend void free(MyPointer <T> &t){
        
        if (t.tombstone->data != nullptr){
            delete t.tombstone->data;
            t.tombstone->data = nullptr;
            t.tombstone->dead = true;
        }
        --t.tombstone->count;
        t.tombstone = nullptr;
    } // delete pointed-at object
      // This is essentially the inverse of the new inside the call to
      // the bootstrapping constructor.It should delete the pointed-to
      // object (which should in turn call its destructor).
      // equality comparisons:

    bool operator==(const MyPointer <T> &t) const { return t.tombstone->data == tombstone->data; }

    bool operator!=(const MyPointer <T> &t) const { return t.tombstone->data != tombstone->data; }

    bool operator==(const int n) const { return (tombstone->data == nullptr) && (n == 0); }
    // true iff MyPointer  is null and int is zero
    
    bool operator!=(const int n) const { return (tombstone->data != nullptr) && (n != 0); }
    // false iff MyPointer  is null and int is zero

    Tombstone<T> *tombstone; // Tombstone MyPointer 
};

template <class T>
bool operator==(const int n, const MyPointer <T> &t) { return t == n; }

template <class T>
bool operator!=(const int n, const MyPointer <T> &t) { return t != n; }

#endif // TOMBSTONES_H