{-# LANGUAGE OverloadedStrings #-}

module LsUpg.Component.Nix.Test (tests) where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (lsupg)
import qualified LsUpg.Component as Component
import qualified LsUpg.Component.Nix as Nix

------------------------------------------------------------------------------

noneOutput :: ByteString
noneOutput = BSL8.unlines
    [ "(dry run; not doing anything)"
    ]

testNone :: TestTree
testNone = testCase "none" $
    ([], []) @=? Nix.parseItems noneOutput

------------------------------------------------------------------------------

upgradeOutput :: ByteString
upgradeOutput = BSL8.unlines
    [ "(dry run; not doing anything)"
    , "upgrading 'nix-2.3.11' to 'nix-2.3.12'"
    , "these paths will be fetched (102.40 MiB download, 302.05 MiB unpacked):"
    , "  /nix/store/0vkw1m51q34dr64z5i87dy99an4hfmyg-coreutils-8.32"
    , "  /nix/store/2cymwbc8hs1yj0k356gjfygf6zf79bm8-libseccomp-2.5.1-lib"
    , "  /nix/store/37ba8404546wplj8crk6y9wjx503p3s5-attr-2.4.48"
    , "  /nix/store/4h03bnxv1c21yzl4vff0z8h2gh55k07y-xz-5.2.5-bin"
    , "  /nix/store/4w3myda7qnfagcmdny4gcxfr38rnr5za-boehm-gc-8.0.4"
    , "  /nix/store/54klr10i53jdfgn7322mzgza6wsai0q8-gcc-10.3.0-lib"
    , "  /nix/store/5hbvlwf0p1h4a3v6jsp00848s0b2yfnf-aws-checksums-0.1.11"
    , "  /nix/store/5k0s057y3swq5cqp58m8p4drq06nfd6w-sqlite-3.35.2"
    , "  /nix/store/5ymjz97754jc6alp50cq1i3iv0jbg8b2-bzip2-1.0.6.0.2"
    , "  /nix/store/65ys3k6gn2s27apky0a0la7wryg3az9q-zlib-1.2.11"
    , "  /nix/store/6kmz38dk2dyr0p4bh0i08h40vfcqfi2a-aws-c-io-0.9.1"
    , "  /nix/store/7c8z6i3nyy0fmg1lcgi3zvdpamnnn4h2-aws-c-common-0.5.11"
    , "  /nix/store/9hxb506q8285gckhdacr72qx3zlkxrl6-gzip-1.10"
    , "  /nix/store/a4yw1svqqk4d8lhwinn9xp847zz9gfma-bash-4.4-p23"
    , "  /nix/store/am5qwbpriqhp1i9qhp2idid7ympxqb9a-glibc-2.32-46-dev"
    , "  /nix/store/ams1l3qrbjc9i2nnnc7fpia5xdz8dq3l-editline-1.17.1"
    , "  /nix/store/b5r82ggd1iy0xh2xdssfcm23l51r2imc-s2n-tls-1.0.1"
    , "  /nix/store/by2bh6wawngyxw4rha6l8yy5slq7pplc-boehm-gc-8.0.4-dev"
    , "  /nix/store/c8m2rn6fh4rh0dbf4bk50fz0qndlhd90-libkrb5-1.18"
    , "  /nix/store/d1bqqd7k5i4ph7p2v6k62p6g4nj3cjv8-curl-7.76.1"
    , "  /nix/store/d1wl54wbfg9dyjah5n2gmlmhd4n8jfbx-brotli-1.0.9-lib"
    , "  /nix/store/d32ym7m2p7lfb6gsghq1dhi61f694k0f-glibc-2.32-46-bin"
    , "  /nix/store/dn0djw0q49pp2fnp6v3s7mk78v63swic-acl-2.3.0"
    , "  /nix/store/dzyimsdk9yq7x6g24r79ipg3vbalyyy1-libidn2-2.3.1"
    , "  /nix/store/fqi6xfddlgafbq1q2lw6z8ysx6vs9yjc-linux-headers-5.12"
    , "  /nix/store/g6d9wwgrhgkali1hisvjbg8p7661cp53-libsodium-1.0.18"
    , "  /nix/store/gx142palm8g6vny225w5nxh9lpz2lv3c-nix-2.3.12-doc"
    , "  /nix/store/gyh4ind4dhwvrxbjxyhz70dz11l5ldfn-aws-sdk-cpp-1.8.121"
    , "  /nix/store/h3f8rn6wwanph9m3rc1gl0lldbr57w3l-gcc-10.3.0"
    , "  /nix/store/h7mvarqbcxcf4lhlwh7csfqgswwl0vvw-keyutils-1.6.3-lib"
    , "  /nix/store/hbm0951q7xrl4qd0ccradp6bhjayfi4b-openssl-1.1.1k"
    , "  /nix/store/i1dc1ac2hxjfl59rvsj49vvgvl1nl16s-libunistring-0.9.10"
    , "  /nix/store/iyqah3h6ywjdxl6xmsdxqv26x71i091v-busybox-static-x86_64-unknown-linux-musl-1.32.1"
    , "  /nix/store/jp2p2frapnpqra774dhd5dwbig102mdm-aws-c-cal-0.4.5"
    , "  /nix/store/khg2dgfr8bq101wvf6zva7c4yapvb4hg-nix-2.3.12-debug"
    , "  /nix/store/liyva04hrpdrrhd4c5fy06avd26wf9ar-nix-2.3.12-man"
    , "  /nix/store/llghzmz573wg22v9rmi4lxh7q36grgh8-nix-2.3.12-dev"
    , "  /nix/store/lrdxgxclyikkan108h19slxlgmkfsl7m-nghttp2-1.43.0-lib"
    , "  /nix/store/njkm9z03dv77my6my4s4b0kchzk166g2-nix-2.3.12"
    , "  /nix/store/rc34ffh62g42vavbsiw5aididd1dmwl4-gnutar-1.34"
    , "  /nix/store/rdslqn6gj1a27laa1xcn0hm147v5an7z-xz-5.2.5"
    , "  /nix/store/sbbifs2ykc05inws26203h0xwcadnf0l-glibc-2.32-46"
    , "  /nix/store/wqgk4p3hch2mz8yl7giy4dm0yk3n89gf-bzip2-1.0.6.0.2-bin"
    , "  /nix/store/ymy44cnid5im2mp9gr9h5j9m3cmkvy1z-libssh2-1.9.0"
    , "  /nix/store/yvvqr0ygv93a98wayaqs4slbdmc6piv0-aws-c-event-stream-0.2.7"
    ]

upgradeItem :: Component.Item
upgradeItem = Component.Item
    { Component.componentName    = Nix.name
    , Component.itemName         = "nix"
    , Component.installedVersion = Just "2.3.11"
    , Component.availableVersion = Just "2.3.12"
    }

testUpgrade :: TestTree
testUpgrade = testCase "upgrade" $
    ([], [upgradeItem]) @=? Nix.parseItems upgradeOutput

------------------------------------------------------------------------------

errorOutput :: ByteString
errorOutput = BSL8.unlines
    [ "(dry run; not doing anything)"
    , "upgrading 'nix-2.3.11' to 'nix-2.3.12'"
    , "upgrading error invalid line"
    , "these paths will be fetched (102.40 MiB download, 302.05 MiB unpacked):"
    , "  /nix/store/0vkw1m51q34dr64z5i87dy99an4hfmyg-coreutils-8.32"
    , "  /nix/store/2cymwbc8hs1yj0k356gjfygf6zf79bm8-libseccomp-2.5.1-lib"
    , "  /nix/store/37ba8404546wplj8crk6y9wjx503p3s5-attr-2.4.48"
    , "  /nix/store/4h03bnxv1c21yzl4vff0z8h2gh55k07y-xz-5.2.5-bin"
    , "  /nix/store/4w3myda7qnfagcmdny4gcxfr38rnr5za-boehm-gc-8.0.4"
    , "  /nix/store/54klr10i53jdfgn7322mzgza6wsai0q8-gcc-10.3.0-lib"
    , "  /nix/store/5hbvlwf0p1h4a3v6jsp00848s0b2yfnf-aws-checksums-0.1.11"
    , "  /nix/store/5k0s057y3swq5cqp58m8p4drq06nfd6w-sqlite-3.35.2"
    , "  /nix/store/5ymjz97754jc6alp50cq1i3iv0jbg8b2-bzip2-1.0.6.0.2"
    , "  /nix/store/65ys3k6gn2s27apky0a0la7wryg3az9q-zlib-1.2.11"
    , "  /nix/store/6kmz38dk2dyr0p4bh0i08h40vfcqfi2a-aws-c-io-0.9.1"
    , "  /nix/store/7c8z6i3nyy0fmg1lcgi3zvdpamnnn4h2-aws-c-common-0.5.11"
    , "  /nix/store/9hxb506q8285gckhdacr72qx3zlkxrl6-gzip-1.10"
    , "  /nix/store/a4yw1svqqk4d8lhwinn9xp847zz9gfma-bash-4.4-p23"
    , "  /nix/store/am5qwbpriqhp1i9qhp2idid7ympxqb9a-glibc-2.32-46-dev"
    , "  /nix/store/ams1l3qrbjc9i2nnnc7fpia5xdz8dq3l-editline-1.17.1"
    , "  /nix/store/b5r82ggd1iy0xh2xdssfcm23l51r2imc-s2n-tls-1.0.1"
    , "  /nix/store/by2bh6wawngyxw4rha6l8yy5slq7pplc-boehm-gc-8.0.4-dev"
    , "  /nix/store/c8m2rn6fh4rh0dbf4bk50fz0qndlhd90-libkrb5-1.18"
    , "  /nix/store/d1bqqd7k5i4ph7p2v6k62p6g4nj3cjv8-curl-7.76.1"
    , "  /nix/store/d1wl54wbfg9dyjah5n2gmlmhd4n8jfbx-brotli-1.0.9-lib"
    , "  /nix/store/d32ym7m2p7lfb6gsghq1dhi61f694k0f-glibc-2.32-46-bin"
    , "  /nix/store/dn0djw0q49pp2fnp6v3s7mk78v63swic-acl-2.3.0"
    , "  /nix/store/dzyimsdk9yq7x6g24r79ipg3vbalyyy1-libidn2-2.3.1"
    , "  /nix/store/fqi6xfddlgafbq1q2lw6z8ysx6vs9yjc-linux-headers-5.12"
    , "  /nix/store/g6d9wwgrhgkali1hisvjbg8p7661cp53-libsodium-1.0.18"
    , "  /nix/store/gx142palm8g6vny225w5nxh9lpz2lv3c-nix-2.3.12-doc"
    , "  /nix/store/gyh4ind4dhwvrxbjxyhz70dz11l5ldfn-aws-sdk-cpp-1.8.121"
    , "  /nix/store/h3f8rn6wwanph9m3rc1gl0lldbr57w3l-gcc-10.3.0"
    , "  /nix/store/h7mvarqbcxcf4lhlwh7csfqgswwl0vvw-keyutils-1.6.3-lib"
    , "  /nix/store/hbm0951q7xrl4qd0ccradp6bhjayfi4b-openssl-1.1.1k"
    , "  /nix/store/i1dc1ac2hxjfl59rvsj49vvgvl1nl16s-libunistring-0.9.10"
    , "  /nix/store/iyqah3h6ywjdxl6xmsdxqv26x71i091v-busybox-static-x86_64-unknown-linux-musl-1.32.1"
    , "  /nix/store/jp2p2frapnpqra774dhd5dwbig102mdm-aws-c-cal-0.4.5"
    , "  /nix/store/khg2dgfr8bq101wvf6zva7c4yapvb4hg-nix-2.3.12-debug"
    , "  /nix/store/liyva04hrpdrrhd4c5fy06avd26wf9ar-nix-2.3.12-man"
    , "  /nix/store/llghzmz573wg22v9rmi4lxh7q36grgh8-nix-2.3.12-dev"
    , "  /nix/store/lrdxgxclyikkan108h19slxlgmkfsl7m-nghttp2-1.43.0-lib"
    , "  /nix/store/njkm9z03dv77my6my4s4b0kchzk166g2-nix-2.3.12"
    , "  /nix/store/rc34ffh62g42vavbsiw5aididd1dmwl4-gnutar-1.34"
    , "  /nix/store/rdslqn6gj1a27laa1xcn0hm147v5an7z-xz-5.2.5"
    , "  /nix/store/sbbifs2ykc05inws26203h0xwcadnf0l-glibc-2.32-46"
    , "  /nix/store/wqgk4p3hch2mz8yl7giy4dm0yk3n89gf-bzip2-1.0.6.0.2-bin"
    , "  /nix/store/ymy44cnid5im2mp9gr9h5j9m3cmkvy1z-libssh2-1.9.0"
    , "  /nix/store/yvvqr0ygv93a98wayaqs4slbdmc6piv0-aws-c-event-stream-0.2.7"
    ]

testError :: TestTree
testError = testCase "error" $
    ([err], [upgradeItem]) @=? Nix.parseItems errorOutput
  where
    err :: String
    err = "error parsing nix line: upgrading error invalid line"

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Nix"
    [ testNone
    , testUpgrade
    , testError
    ]
