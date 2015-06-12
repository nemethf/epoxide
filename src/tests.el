(require 'epoxide)
(require 'ert)

(ert-deftest epoxide-ipv4-address-p ()
  "Test for epoxide-ipv4-address-p."
  (should (epoxide-ipv4-address-p "156.22.33.44"))
  (should (epoxide-ipv4-address-p "0.0.0.0"))
  (should-not (epoxide-ipv4-address-p "256.22.44"))
  (should-not (epoxide-ipv4-address-p "156.22.33.44.4"))
  (should-not (epoxide-ipv4-address-p "156.22..44"))
  (should-not (epoxide-ipv4-address-p "156.22.33.443"))
  (should-not (epoxide-ipv4-address-p "x156.22.33.44")))
