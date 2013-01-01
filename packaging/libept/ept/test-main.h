// -*- C++ -*-
#include <unistd.h>
#include <sys/wait.h>

struct Main {

    int suite, test;
    int status[2];
    int confirm[2];
    FILE *f_confirm, *f_status;
    pid_t pid;
    int argc;
    char **argv;
    pid_t finished;
    int status_code;
    int test_ok;

    int suite_ok, suite_failed;
    int total_ok, total_failed;

    int announced_suite;
    std::string current;

    RunAll all;

    Main() : suite(0), test(0) {
        suite_ok = suite_failed = 0;
        total_ok = total_failed = 0;
        test_ok = 0;
        announced_suite = -1;
    }

    void child() {
        close( status[0] );
        close( confirm[1] );
        all.status = fdopen( status[1], "w" );
        all.confirm = fdopen( confirm[0], "r" );
        if ( argc > 1 ) {
            RunSuite *s = all.findSuite( argv[1] );
            if (!s) {
                std::cerr << "No such suite " << argv[1] << std::endl;
                // todo dump possible suites?
                exit(250);
            }
            all.runSuite( *s, test, 0, 1 );
        }
        if ( argc == 1 ) {
            all.runFrom( suite, test );
        }
        fprintf( all.status, "done\n" );
        exit( 0 );
    }

    void testDied()
    {
        /* std::cerr << "test died: " << test << "/"
           << suites[suite].testCount << std::endl; */
        if ( WIFEXITED( status_code ) ) {
            if ( WEXITSTATUS( status_code ) == 250 )
                exit( 3 );
            if ( WEXITSTATUS( status_code ) == 0 )
                return;
        }
        std::cout << "failed test: "<< current;
        if ( WIFEXITED( status_code ) )
            std::cout << " (exit status " << WEXITSTATUS( status_code ) << ")";
        if ( WIFSIGNALED( status_code ) )
            std::cout << " (caught signal " << WTERMSIG( status_code ) << ")";
        std::cout << std::endl;
        // re-announce the suite
        announced_suite --;
        ++ test; // continue with next test
        test_ok = 0;
        suite_failed ++;
    }

    void processStatus( const char *line ) {
        if ( std::string("done") == line ) { // finished
            finished = waitpid( pid, &status_code, 0 );
            assert_eq( pid, finished );
            assert( WIFEXITED( status_code ) );
            assert_eq( WEXITSTATUS( status_code ), 0 );
            std::cout << "overall " << total_ok << "/"
                      << total_ok + total_failed
                      << " ok" << std::endl;
            exit( total_failed == 0 ? 0 : 1 );
        }

        if ( test_ok ) {
            /* std::cerr << "test ok: " << test << "/"
               << suites[suite].testCount << std::endl; */
            std::cout << "." << std::flush;
            suite_ok ++;
            ++ test;
            test_ok = 0;
        }

        if ( line[0] == 's' ) {
            if ( line[2] == 'd' ) {
                std::cout << " " << suite_ok << "/" << suite_ok + suite_failed
                          << " ok" << std::endl;
                ++ suite; test = 0;
                assert( !test_ok );
                total_ok += suite_ok;
                total_failed += suite_failed;
                suite_ok = suite_failed = 0;
            }
            if ( line[2] == 's' ) {
                if ( announced_suite < suite ) {
                    std::cout << line + 5 << ": " << std::flush;
                    announced_suite = suite;
                }
            }
        }
        if ( line[0] == 't' ) {
            if ( line[2] == 'd' ) {
                fprintf( f_confirm, "ack\n" );
                fflush( f_confirm );
                test_ok = 1;
            }
            if ( line[2] == 's' ) {
                fprintf( f_confirm, "ack\n" );
                fflush( f_confirm );
                current = line + 5;
            }
        }
    }

    void parent() {
        close( status[1] );
        close( confirm[0] );
        f_status = fdopen( status[0], "r" );
        f_confirm = fdopen( confirm[1], "w" );
        char *line = 0;
        size_t n;

        while ( true ) {
            if ( getline( &line, &n, f_status ) < 0 ) {
                finished = waitpid( pid, &status_code, 0 );
                if ( finished < 0 ) {
                    perror( "waitpid failed" );
                    exit( 5 );
                }
                assert_eq( pid, finished );
                testDied();
                /* std::cerr << "child will be reforked at: "
                   << suite << " " << test << std::endl; */
                return;
            } else {
                // std::cerr << "reading pipe: " << line;
                line[ strlen( line ) - 1 ] = 0;
                processStatus( line );
                free( line );
            }
            line = 0;
        }
    }

    int main( int _argc, char **_argv )
    {
        argc = _argc;
        argv = _argv;

        all.suiteCount = sizeof(suites)/sizeof(RunSuite);
        all.suites = suites;

        while (true) {
            if ( pipe( status ) )
                return 1;
            if ( pipe( confirm ) )
                return 1;
            pid = fork();
            if ( pid < 0 )
                return 2;
            if ( pid == 0 ) { // child
                child();
            } else {
                parent();
            }
        }
    }
};

int main( int argc, char **argv ) {
    return Main().main( argc, argv );
}
