import apt_pkg


def main():
    apt_pkg.init_config()

    print "Native architecture:", apt_pkg.config["APT::Architecture"]
    print "All architectures:", apt_pkg.config.value_list("APT::Architectures")
    

if __name__ == '__main__':
    main()
