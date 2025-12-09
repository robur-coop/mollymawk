# Mollymawk

From Wikipedia: The mollymawks are a group of medium-sized albatrosses that form the genus Thalassarche.

A web interface for remotely managing [albatross](https://github.com/robur-coop/albatross).

Both as an interactive user interface to point and click deploying and destroying unikernels, as well as a REST API with authentication.

Mollymawk is a unikernel itself, and communicates via TLS with albatross. It preserves state about users and configuration for the remote albatross instances.

The assumption is that only mollymawk maintains and modifies the albatross that is running - it is not supported that you run other administrative tasks (such as modifying the policies) while mollymawk is running.

## Documentation

You can find a rendered version of our handbook for mollymawk at https://robur-coop.github.io/mollymawk-handbook/index.html

## Funding

This project is funded through [NGI Zero Core](https://nlnet.nl/core), a fund established by [NLnet](https://nlnet.nl) with financial support from the European Commission's [Next Generation Internet](https://ngi.eu) program. Learn more at the [NLnet project page](https://nlnet.nl/project/Mollymawk).
