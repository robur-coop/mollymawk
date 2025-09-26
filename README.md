# Mollymawk

From Wikipedia: The mollymawks are a group of medium-sized albatrosses that form the genus Thalassarche.

A web interface for remotely managing [albatross](https://github.com/robur-coop/albatross).

Both as an interactive user interface to point and click deploying and destroying unikernels, as well as a REST API with authentication.

Mollymawk is a unikernel itself, and communicates via TLS with albatross. It preserves state about users and configuration for the remote albatross instances.

The assumption is that only mollymawk maintains and modifies the albatross that is running - it is not supported that you run other administrative tasks (such as modifying the policies) while mollymawk is running.

## Using Mollymawk with multiple Albatross installations

With **Mollymawk**, you can manage **multiple Albatross** instances.

---

### Information needed per instance

* **Name** – Friendly label (e.g., `prod-eu-1`)
* **IP** – e.g., `10.0.42.15`
* **Port** – Default for albatross is **1025**
* **Certificate** – The contents of the certificate file. See [*Generating an Albatross certificate and key*](https://github.com/robur-coop/albatross?tab=readme-ov-file#setup)
* **Key** – The contents of the certificate's key file. See [*Generating an Albatross certificate and key*](https://github.com/robur-coop/albatross?tab=readme-ov-file#setup)

---

### Add an instance (step-by-step)

1. In Mollymawk, open **Settings**.
2. Click the **`[ + ]`** button on the top right corner.
4. Fill the form:

   * **Name** (e.g., `prod-eu-1`)
   * **IP** (e.g., `10.0.42.15`)
   * **Port** (e.g., `1025`)
   * **Certificate** – paste the certificate file content
   * **Key** – paste key file content
5. Click **Save**. The instance appears after a successful connection.

---

## Funding

This project is funded through [NGI Zero Core](https://nlnet.nl/core), a fund established by [NLnet](https://nlnet.nl) with financial support from the European Commission's [Next Generation Internet](https://ngi.eu) program. Learn more at the [NLnet project page](https://nlnet.nl/project/Mollymawk).
