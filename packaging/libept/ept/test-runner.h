#include <unistd.h>

#define RUN(x, y) x().y()

struct RunTest {
    const char *name;
    void (*run)();
};

struct RunSuite {
    const char *name;
    RunTest *tests;
    int testCount;
};

struct RunAll {
    RunSuite *suites;
    int suiteCount;
    FILE *status, *confirm;

    RunSuite *findSuite( std::string name ) {
        for ( int i = 0; i < suiteCount; ++i )
            if ( suites[i].name == name )
                return suites + i;
        return 0;
    }

    void waitForAck() {
        size_t n = 0; char *line = 0;
        size_t read = getline( &line, &n, confirm );
        assert_eq( read, 4 );
        assert_eq( std::string( "ack\n" ), line );
        free( line );
    }

    void runSuite( RunSuite &s, int fromTest, int suite, int suiteCount )
    {
        fprintf( status, "s/s: (%d/%d) %s\n", suite + 1, suiteCount, s.name );
        for ( int i = fromTest; i < s.testCount; ++i ) {
            fprintf( status, "t/s: (%d/%d) %s\n", i, s.testCount,
                     s.tests[i].name );
            fflush( status );
            waitForAck();
            s.tests[i].run();
            fprintf( status, "t/d: %s\n", s.tests[i].name );
            fflush( status );
            waitForAck();
            // exit( 0 ); // TODO make this optional; safety vs
                       // performance tradeoff
        }
        fprintf( status, "s/d: %s\n", s.name );
    }

    void runFrom( int suite, int test )
    {
        for ( int i = suite; i < suiteCount; ++i ) {
            assert( suite <= suiteCount );
            runSuite( suites[i], test, i, suiteCount );
            test = 0;
        }
    }
};

