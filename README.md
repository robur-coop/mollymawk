# mollymawk

From Wikipedia: The mollymawks are a group of medium-sized albatrosses that form the genus Thalassarche.

A web interface for remotely managing [albatross](https://github.com/robur-coop/albatross).

Both as an interactive user interface to point and click deploying and destroying unikernels, as well as a REST API with authentication.

Mollymawk is a unikernel itself, and communicates via TLS with albatross. It preserves state about users and configuration for the remote albatross instance.

The assumption is that only mollymawk maintains and modifies the albatross that is running - it is not supported that you run other administrative tasks (such as modifying the policies) while mollymawk is running.
