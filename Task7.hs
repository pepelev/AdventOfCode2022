module Task7 where

import Data.List (splitAt, findIndex, intercalate, groupBy, inits, sort, nub, isPrefixOf)

input = "$ ls\n\
\dir bhtvbj\n\
\dir bmlllrl\n\
\dir dhm\n\
\dir mnp\n\
\dir nwqgchw\n\
\$ cd bhtvbj\n\
\$ ls\n\
\dir dmd\n\
\dir fjblqtdp\n\
\25595 mdmtpjq.wmf\n\
\dir qhm\n\
\dir rjr\n\
\dir smtrp\n\
\dir tbdsml\n\
\$ cd dmd\n\
\$ ls\n\
\232616 ngmqbc.mdj\n\
\75367 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd fjblqtdp\n\
\$ ls\n\
\dir czvcf\n\
\dir jnzwf\n\
\245590 lcpgtrc.dqm\n\
\141631 nwqgchw\n\
\37152 nwqgchw.ppg\n\
\80432 rbj.twt\n\
\$ cd czvcf\n\
\$ ls\n\
\dir cqzcp\n\
\dir czvcf\n\
\$ cd cqzcp\n\
\$ ls\n\
\dir bshmsns\n\
\55418 svhphd\n\
\232179 vqqcvgts.vrc\n\
\$ cd bshmsns\n\
\$ ls\n\
\243135 rbhprlgq.gbh\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd czvcf\n\
\$ ls\n\
\158882 lhfsc.lrh\n\
\266626 ntsrpn\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd jnzwf\n\
\$ ls\n\
\108142 btmz\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd qhm\n\
\$ ls\n\
\162986 bnqbdmm.dfh\n\
\dir hqbhr\n\
\201987 hwvdlfl\n\
\dir lvdrr\n\
\143900 nwcjvb\n\
\dir rgtcchh\n\
\297583 wdcsgg.cjt\n\
\$ cd hqbhr\n\
\$ ls\n\
\175196 btmz\n\
\dir fcm\n\
\221851 ngmqbc\n\
\dir qnlssvn\n\
\263872 rdzsz.grd\n\
\dir vvbgt\n\
\dir wwzwqqh\n\
\$ cd fcm\n\
\$ ls\n\
\66471 sfddtgp.flr\n\
\$ cd ..\n\
\$ cd qnlssvn\n\
\$ ls\n\
\dir czvcf\n\
\dir gngvc\n\
\75812 lbthznl.llq\n\
\182104 nwqgchw.nlq\n\
\161446 rrvwdw.nzv\n\
\dir rzssqpcj\n\
\260877 wdcsgg.cjt\n\
\$ cd czvcf\n\
\$ ls\n\
\202850 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd gngvc\n\
\$ ls\n\
\154834 rdzsz.hst\n\
\$ cd ..\n\
\$ cd rzssqpcj\n\
\$ ls\n\
\66116 bdzdp\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd vvbgt\n\
\$ ls\n\
\288775 cpsmvwq\n\
\dir fbfddwqz\n\
\81857 jcpj.wpf\n\
\dir nwjps\n\
\49905 tlrlbg.mgz\n\
\64870 wdcsgg.cjt\n\
\131013 zbc.rhl\n\
\$ cd fbfddwqz\n\
\$ ls\n\
\100619 wdcsgg.cjt\n\
\$ cd ..\n\
\$ cd nwjps\n\
\$ ls\n\
\96526 cjrvb.tdv\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd wwzwqqh\n\
\$ ls\n\
\dir czvcf\n\
\$ cd czvcf\n\
\$ ls\n\
\256121 nrjfpjc.wcg\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd lvdrr\n\
\$ ls\n\
\27488 lhfsc.lrh\n\
\$ cd ..\n\
\$ cd rgtcchh\n\
\$ ls\n\
\19285 bfdfz.rln\n\
\166070 btmz\n\
\222301 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd rjr\n\
\$ ls\n\
\dir dbb\n\
\121292 drv.ljf\n\
\dir ntbbd\n\
\228025 rphtjh.ngl\n\
\133033 wrlwdgz\n\
\$ cd dbb\n\
\$ ls\n\
\158756 btmz\n\
\130326 czvcf.trn\n\
\dir fdgh\n\
\dir grr\n\
\20181 wdcsgg.cjt\n\
\$ cd fdgh\n\
\$ ls\n\
\24629 rph.rsl\n\
\299233 wdcsgg.cjt\n\
\$ cd ..\n\
\$ cd grr\n\
\$ ls\n\
\259732 tqvvp\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ntbbd\n\
\$ ls\n\
\dir cwwhvghw\n\
\dir hggcq\n\
\169994 jrvt.srj\n\
\dir jtzbw\n\
\dir ptr\n\
\215668 smcngpwr\n\
\dir tfshcbw\n\
\$ cd cwwhvghw\n\
\$ ls\n\
\dir czvcf\n\
\167719 dzltv\n\
\dir mdgqwdjq\n\
\265831 pcfcw.jrd\n\
\86965 qsdv\n\
\71709 tdbtjwzp.msg\n\
\dir vtbr\n\
\$ cd czvcf\n\
\$ ls\n\
\dir ntj\n\
\dir nwqgchw\n\
\dir rdzsz\n\
\202867 vqqcvgts.vrc\n\
\$ cd ntj\n\
\$ ls\n\
\214072 ntplhvnn.zpt\n\
\$ cd ..\n\
\$ cd nwqgchw\n\
\$ ls\n\
\228489 qfphslzt\n\
\15383 zfpdpds.bjt\n\
\$ cd ..\n\
\$ cd rdzsz\n\
\$ ls\n\
\dir jrvt\n\
\6415 jrvt.vjt\n\
\290773 mhfwsc.nlr\n\
\82027 wdcsgg.cjt\n\
\$ cd jrvt\n\
\$ ls\n\
\85079 mnq.jvr\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd mdgqwdjq\n\
\$ ls\n\
\223814 phghj\n\
\172175 wwpvcb\n\
\$ cd ..\n\
\$ cd vtbr\n\
\$ ls\n\
\134023 frwc.dhg\n\
\26692 fvgscmns.mpj\n\
\148404 wnlgfmdr.dch\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd hggcq\n\
\$ ls\n\
\19064 btmz\n\
\200043 lswzn\n\
\dir mlrj\n\
\49427 rcwmzz.nsn\n\
\dir wjznmcw\n\
\$ cd mlrj\n\
\$ ls\n\
\10869 czvcf.fvc\n\
\277796 gprlsg.tbt\n\
\$ cd ..\n\
\$ cd wjznmcw\n\
\$ ls\n\
\43168 bzwn\n\
\dir dznz\n\
\4102 lcpgtrc.dqm\n\
\dir ltcpgcdf\n\
\228100 nwqgchw.mgc\n\
\dir tbdqsnb\n\
\dir tmzswrgt\n\
\19984 whgmsm.bjj\n\
\$ cd dznz\n\
\$ ls\n\
\59403 msqdt.mlm\n\
\$ cd ..\n\
\$ cd ltcpgcdf\n\
\$ ls\n\
\dir czvcf\n\
\10727 jgphjm.pdw\n\
\309167 nwqgchw\n\
\dir pwbt\n\
\dir qznbn\n\
\203154 ztpcdmb.rrs\n\
\$ cd czvcf\n\
\$ ls\n\
\173609 pntjz.vzq\n\
\292292 trvbpz.djc\n\
\111008 wdcsgg.cjt\n\
\107437 wjvv.hsj\n\
\265353 wsbff.pzh\n\
\$ cd ..\n\
\$ cd pwbt\n\
\$ ls\n\
\307172 jtdtlbsh\n\
\dir lgz\n\
\$ cd lgz\n\
\$ ls\n\
\dir fnlsq\n\
\$ cd fnlsq\n\
\$ ls\n\
\161356 jrvt.ljb\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd qznbn\n\
\$ ls\n\
\12354 bdgvj\n\
\59582 cslzb.qnq\n\
\dir czvcf\n\
\dir psnpf\n\
\136432 vlsswwgv\n\
\99861 vqqcvgts.vrc\n\
\39898 wzllwpmr.mqc\n\
\$ cd czvcf\n\
\$ ls\n\
\dir dqnpjjrv\n\
\13244 ztcpzzr\n\
\$ cd dqnpjjrv\n\
\$ ls\n\
\233675 ngmqbc.srp\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd psnpf\n\
\$ ls\n\
\1986 lhfsc.lrh\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd tbdqsnb\n\
\$ ls\n\
\170099 hbcnv.gmj\n\
\$ cd ..\n\
\$ cd tmzswrgt\n\
\$ ls\n\
\118969 btmz\n\
\dir czvcf\n\
\51649 hbb.jcb\n\
\$ cd czvcf\n\
\$ ls\n\
\163330 hhcf\n\
\159514 wdhw\n\
\22876 wtn.pnb\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd jtzbw\n\
\$ ls\n\
\55634 rdzsz.dgf\n\
\$ cd ..\n\
\$ cd ptr\n\
\$ ls\n\
\49447 wdcsgg.cjt\n\
\$ cd ..\n\
\$ cd tfshcbw\n\
\$ ls\n\
\193480 btmz\n\
\44402 sjsgfmts.dtc\n\
\115952 vqncb.ssf\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd smtrp\n\
\$ ls\n\
\dir hlpzdbwp\n\
\307696 jrvt.hds\n\
\300691 lcpgtrc.dqm\n\
\dir nflt\n\
\dir qcph\n\
\dir qlrdf\n\
\$ cd hlpzdbwp\n\
\$ ls\n\
\dir czvcf\n\
\dir frzvnrb\n\
\dir jrvt\n\
\$ cd czvcf\n\
\$ ls\n\
\155141 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd frzvnrb\n\
\$ ls\n\
\71241 btmz\n\
\dir rdzsz\n\
\dir vdb\n\
\$ cd rdzsz\n\
\$ ls\n\
\35362 jngsmcrm.pwt\n\
\$ cd ..\n\
\$ cd vdb\n\
\$ ls\n\
\239928 jrvt.nbf\n\
\16883 ngmqbc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd jrvt\n\
\$ ls\n\
\dir lcchtcz\n\
\$ cd lcchtcz\n\
\$ ls\n\
\199091 qzsh.fst\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd nflt\n\
\$ ls\n\
\219987 nwqgchw.qpf\n\
\dir rdzsz\n\
\257069 wdcsgg.cjt\n\
\$ cd rdzsz\n\
\$ ls\n\
\dir zmgf\n\
\$ cd zmgf\n\
\$ ls\n\
\dir vwcvbff\n\
\$ cd vwcvbff\n\
\$ ls\n\
\157598 qsp\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd qcph\n\
\$ ls\n\
\158708 cdj.bch\n\
\dir drdpdzj\n\
\dir jrvt\n\
\109459 lhfsc.lrh\n\
\dir ngmqbc\n\
\164488 rtnvpg\n\
\23729 vqqcvgts.vrc\n\
\115775 wdcsgg.cjt\n\
\$ cd drdpdzj\n\
\$ ls\n\
\dir ngwcr\n\
\dir pwffm\n\
\dir vcclwq\n\
\$ cd ngwcr\n\
\$ ls\n\
\76003 rqjbn\n\
\84407 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd pwffm\n\
\$ ls\n\
\284565 rzdjrmn.jdz\n\
\$ cd ..\n\
\$ cd vcclwq\n\
\$ ls\n\
\137044 czvcf.qll\n\
\3433 nwqgchw\n\
\18027 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd jrvt\n\
\$ ls\n\
\57605 vljs\n\
\$ cd ..\n\
\$ cd ngmqbc\n\
\$ ls\n\
\217554 btmz\n\
\96485 lhfsc.lrh\n\
\dir lpcr\n\
\dir sltwgmjv\n\
\dir snll\n\
\dir tsq\n\
\218323 vqqcvgts.vrc\n\
\150009 wdcsgg.cjt\n\
\$ cd lpcr\n\
\$ ls\n\
\227650 qhfz.grc\n\
\$ cd ..\n\
\$ cd sltwgmjv\n\
\$ ls\n\
\39536 fncjl.vlr\n\
\248067 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd snll\n\
\$ ls\n\
\70368 btmz\n\
\195228 svmdc.pcv\n\
\$ cd ..\n\
\$ cd tsq\n\
\$ ls\n\
\271904 vqqcvgts.vrc\n\
\104043 wbgwpcl\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd qlrdf\n\
\$ ls\n\
\dir dqgln\n\
\dir ngmqbc\n\
\dir ntngh\n\
\$ cd dqgln\n\
\$ ls\n\
\dir qdrszjvm\n\
\$ cd qdrszjvm\n\
\$ ls\n\
\199245 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ngmqbc\n\
\$ ls\n\
\171851 czvcf.jft\n\
\dir ngmqbc\n\
\dir qdffn\n\
\$ cd ngmqbc\n\
\$ ls\n\
\14596 jjhmhzs.dww\n\
\$ cd ..\n\
\$ cd qdffn\n\
\$ ls\n\
\dir czvcf\n\
\$ cd czvcf\n\
\$ ls\n\
\130227 jhqhd.fdz\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ntngh\n\
\$ ls\n\
\243908 bqjfjnl.pcl\n\
\112351 btmz\n\
\30167 lcpgtrc.dqm\n\
\249181 mfwcvc.zdg\n\
\dir qlhw\n\
\157482 vqqcvgts.vrc\n\
\$ cd qlhw\n\
\$ ls\n\
\267233 gfhthp.prr\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd tbdsml\n\
\$ ls\n\
\44152 btmz\n\
\143454 cpzlrsh\n\
\47848 crdvhbt.dfr\n\
\dir gvjhlqdd\n\
\171842 mgljcrw.trm\n\
\dir nqsq\n\
\dir rdzsz\n\
\$ cd gvjhlqdd\n\
\$ ls\n\
\177040 ffbm\n\
\$ cd ..\n\
\$ cd nqsq\n\
\$ ls\n\
\dir fchtl\n\
\dir jrvt\n\
\dir nsgbjwbt\n\
\dir qcz\n\
\dir vqlnqvwn\n\
\55184 wlspgz\n\
\dir wzm\n\
\dir zpw\n\
\$ cd fchtl\n\
\$ ls\n\
\193793 btmz\n\
\164089 jrvt.hzn\n\
\53839 lpv.gtg\n\
\dir qmfwds\n\
\dir sqznc\n\
\dir tdqg\n\
\dir zvd\n\
\$ cd qmfwds\n\
\$ ls\n\
\dir dqtbp\n\
\236341 lcpgtrc.dqm\n\
\101548 rdzsz.vqr\n\
\180341 wzpdq.gjr\n\
\$ cd dqtbp\n\
\$ ls\n\
\56177 hdgnthn.dff\n\
\56834 jrvt.nmg\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd sqznc\n\
\$ ls\n\
\282988 dpdfvn.spw\n\
\248737 tzfd.pfd\n\
\$ cd ..\n\
\$ cd tdqg\n\
\$ ls\n\
\251266 rdzsz.dhb\n\
\$ cd ..\n\
\$ cd zvd\n\
\$ ls\n\
\124979 ngmqbc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd jrvt\n\
\$ ls\n\
\206684 dbbppj.sds\n\
\189832 jvst.fzr\n\
\$ cd ..\n\
\$ cd nsgbjwbt\n\
\$ ls\n\
\32810 btmz\n\
\$ cd ..\n\
\$ cd qcz\n\
\$ ls\n\
\dir bjbsl\n\
\dir brvgznjr\n\
\98771 btmz\n\
\dir gbfhz\n\
\dir ngmqbc\n\
\88248 rccpzctp.gwn\n\
\dir rdzsz\n\
\39060 tqswrdh.wfc\n\
\dir ztnv\n\
\$ cd bjbsl\n\
\$ ls\n\
\1312 rdzsz.qtl\n\
\$ cd ..\n\
\$ cd brvgznjr\n\
\$ ls\n\
\98988 lhfsc.lrh\n\
\$ cd ..\n\
\$ cd gbfhz\n\
\$ ls\n\
\96203 hgldz\n\
\28558 nwqgchw\n\
\dir qrzd\n\
\240140 wjww.hjf\n\
\$ cd qrzd\n\
\$ ls\n\
\231108 hjfcwvtq\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ngmqbc\n\
\$ ls\n\
\dir cpjvd\n\
\dir vrbfcwc\n\
\$ cd cpjvd\n\
\$ ls\n\
\142549 btmz\n\
\dir ngmqbc\n\
\$ cd ngmqbc\n\
\$ ls\n\
\62008 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd vrbfcwc\n\
\$ ls\n\
\dir czj\n\
\$ cd czj\n\
\$ ls\n\
\48640 btmz\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd rdzsz\n\
\$ ls\n\
\dir czvcf\n\
\dir fmgmgmp\n\
\dir jrvt\n\
\308389 lhfsc.lrh\n\
\dir nfdh\n\
\dir ptgsd\n\
\dir qmg\n\
\244691 vqqcvgts.vrc\n\
\$ cd czvcf\n\
\$ ls\n\
\dir fqjrb\n\
\dir jbtgpl\n\
\dir jstzjf\n\
\299095 lcpgtrc.dqm\n\
\122426 lhfsc.lrh\n\
\dir wczwphjh\n\
\219013 wdcsgg.cjt\n\
\$ cd fqjrb\n\
\$ ls\n\
\dir fpspthg\n\
\dir hdmtsv\n\
\118041 mwlsw.fvs\n\
\dir rdzsz\n\
\99976 wdcsgg.cjt\n\
\$ cd fpspthg\n\
\$ ls\n\
\280707 hmwsq\n\
\$ cd ..\n\
\$ cd hdmtsv\n\
\$ ls\n\
\102842 btmz\n\
\72949 fpzqpqb.zjp\n\
\$ cd ..\n\
\$ cd rdzsz\n\
\$ ls\n\
\36159 sjtwbsvc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd jbtgpl\n\
\$ ls\n\
\139817 lhfsc.lrh\n\
\139333 nwqgchw.wrz\n\
\$ cd ..\n\
\$ cd jstzjf\n\
\$ ls\n\
\dir gngbnq\n\
\54929 lcpgtrc.dqm\n\
\dir pdbdwmc\n\
\$ cd gngbnq\n\
\$ ls\n\
\dir bfvsz\n\
\dir pndfrjhz\n\
\$ cd bfvsz\n\
\$ ls\n\
\283370 mdf.wvc\n\
\$ cd ..\n\
\$ cd pndfrjhz\n\
\$ ls\n\
\252824 lhfsc.lrh\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd pdbdwmc\n\
\$ ls\n\
\dir nwqgchw\n\
\dir sdmfntl\n\
\266823 vqqcvgts.vrc\n\
\$ cd nwqgchw\n\
\$ ls\n\
\dir bnfhbvmr\n\
\$ cd bnfhbvmr\n\
\$ ls\n\
\62602 lrmmtjmv\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd sdmfntl\n\
\$ ls\n\
\93365 njfgsgm.jtv\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd wczwphjh\n\
\$ ls\n\
\164840 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd fmgmgmp\n\
\$ ls\n\
\292610 jglzqc.mss\n\
\dir rdzsz\n\
\dir rrrjw\n\
\$ cd rdzsz\n\
\$ ls\n\
\295660 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd rrrjw\n\
\$ ls\n\
\dir lsc\n\
\$ cd lsc\n\
\$ ls\n\
\280045 dljtrq.tll\n\
\dir nwqgchw\n\
\$ cd nwqgchw\n\
\$ ls\n\
\162525 lhfsc.lrh\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd jrvt\n\
\$ ls\n\
\227518 pcsqv\n\
\$ cd ..\n\
\$ cd nfdh\n\
\$ ls\n\
\304769 ngmqbc.qhd\n\
\$ cd ..\n\
\$ cd ptgsd\n\
\$ ls\n\
\281593 cpfzhhd\n\
\123541 hhlhssqb.szt\n\
\250182 lcpgtrc.dqm\n\
\dir ngmqbc\n\
\229226 pppmnp\n\
\dir wntfhzqf\n\
\dir zchjnbz\n\
\$ cd ngmqbc\n\
\$ ls\n\
\202162 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd wntfhzqf\n\
\$ ls\n\
\dir czvcf\n\
\dir rdzsz\n\
\dir wvhznt\n\
\$ cd czvcf\n\
\$ ls\n\
\250212 bzwsvd.lhc\n\
\$ cd ..\n\
\$ cd rdzsz\n\
\$ ls\n\
\244145 ngmqbc.lfb\n\
\236278 plnjrm.rgs\n\
\$ cd ..\n\
\$ cd wvhznt\n\
\$ ls\n\
\264719 czvcf.cgn\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd zchjnbz\n\
\$ ls\n\
\dir jrvt\n\
\dir msrs\n\
\dir vtrcs\n\
\$ cd jrvt\n\
\$ ls\n\
\154825 jrvt\n\
\44966 rdzsz\n\
\198819 vnnrqcbr.fjf\n\
\$ cd ..\n\
\$ cd msrs\n\
\$ ls\n\
\188969 cwbq.ltd\n\
\$ cd ..\n\
\$ cd vtrcs\n\
\$ ls\n\
\2014 jmvvq.pvn\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd qmg\n\
\$ ls\n\
\dir dqfs\n\
\dir hwnbws\n\
\dir ngmqbc\n\
\$ cd dqfs\n\
\$ ls\n\
\130929 smwcjg.vjm\n\
\$ cd ..\n\
\$ cd hwnbws\n\
\$ ls\n\
\dir vsq\n\
\$ cd vsq\n\
\$ ls\n\
\196984 twlvvd.qlc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ngmqbc\n\
\$ ls\n\
\212410 cdzjjw\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ztnv\n\
\$ ls\n\
\167568 pwrsss\n\
\64234 rlprpl\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd vqlnqvwn\n\
\$ ls\n\
\19448 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd wzm\n\
\$ ls\n\
\123271 lhfsc.lrh\n\
\dir ngmqbc\n\
\dir qvvvdl\n\
\$ cd ngmqbc\n\
\$ ls\n\
\dir bdjfhmvz\n\
\101745 cqg\n\
\dir ngmqbc\n\
\119605 ngmqbc.lnd\n\
\dir tnfr\n\
\dir wfzct\n\
\$ cd bdjfhmvz\n\
\$ ls\n\
\104287 jrvt.nnj\n\
\$ cd ..\n\
\$ cd ngmqbc\n\
\$ ls\n\
\88793 nwqgchw\n\
\$ cd ..\n\
\$ cd tnfr\n\
\$ ls\n\
\dir nggnpj\n\
\161400 vqqcvgts.vrc\n\
\$ cd nggnpj\n\
\$ ls\n\
\308915 btmz\n\
\81154 jjtwrbtw.bln\n\
\50902 sfppg.hvn\n\
\dir tpg\n\
\$ cd tpg\n\
\$ ls\n\
\143630 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd wfzct\n\
\$ ls\n\
\71154 bzhzl.zcg\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd qvvvdl\n\
\$ ls\n\
\185898 nwqgchw.tvr\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd zpw\n\
\$ ls\n\
\167347 bcfj.lch\n\
\dir fldmgj\n\
\dir jspslmwp\n\
\199949 rdsz.dng\n\
\$ cd fldmgj\n\
\$ ls\n\
\154330 sbftm.wmt\n\
\$ cd ..\n\
\$ cd jspslmwp\n\
\$ ls\n\
\75378 jrvt.jdw\n\
\26174 mzthsl.qtv\n\
\214743 njjdqsr\n\
\29213 tsdnqwj\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd rdzsz\n\
\$ ls\n\
\263486 cfjb.mfc\n\
\77949 fjnfp.lcl\n\
\262618 lcpgtrc.dqm\n\
\124555 lhfsc.lrh\n\
\dir pngmr\n\
\$ cd pngmr\n\
\$ ls\n\
\305791 fdvbthn.cvs\n\
\32332 rdjvldmt.lfw\n\
\dir rwwqsl\n\
\dir rzgv\n\
\$ cd rwwqsl\n\
\$ ls\n\
\158602 bmqnqtz\n\
\dir cvphd\n\
\dir hpb\n\
\$ cd cvphd\n\
\$ ls\n\
\119828 hfhvv.ffp\n\
\dir qbvcjq\n\
\257077 wdcsgg.cjt\n\
\$ cd qbvcjq\n\
\$ ls\n\
\dir nwqgchw\n\
\$ cd nwqgchw\n\
\$ ls\n\
\127576 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd hpb\n\
\$ ls\n\
\176379 nwqgchw\n\
\166831 qhdgmsvv.bdr\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd rzgv\n\
\$ ls\n\
\56544 pngtztnf.gdt\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd bmlllrl\n\
\$ ls\n\
\270146 chqqnfpn.dfs\n\
\dir ngmqbc\n\
\dir rdzsz\n\
\70712 vqqcvgts.vrc\n\
\$ cd ngmqbc\n\
\$ ls\n\
\dir rgcrvvgj\n\
\301804 vqqcvgts.vrc\n\
\$ cd rgcrvvgj\n\
\$ ls\n\
\219577 jwrlwq\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd rdzsz\n\
\$ ls\n\
\290477 nwqgchw.rng\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd dhm\n\
\$ ls\n\
\127736 npznvgqn.bdd\n\
\59221 smmlzfj.lhh\n\
\22345 zhfpvppf.gtn\n\
\$ cd ..\n\
\$ cd mnp\n\
\$ ls\n\
\dir gdntwv\n\
\dir qlsfmcqp\n\
\dir schlsbb\n\
\$ cd gdntwv\n\
\$ ls\n\
\133571 btmz\n\
\$ cd ..\n\
\$ cd qlsfmcqp\n\
\$ ls\n\
\244176 bffzdczp.gqf\n\
\12060 cqlvm.wdd\n\
\dir jnl\n\
\14040 ldczcfl\n\
\dir nwqgchw\n\
\243637 sphmmcv\n\
\290808 wdcsgg.cjt\n\
\$ cd jnl\n\
\$ ls\n\
\252674 lcpgtrc.dqm\n\
\$ cd ..\n\
\$ cd nwqgchw\n\
\$ ls\n\
\258944 btmz\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd schlsbb\n\
\$ ls\n\
\dir fpbrwnz\n\
\dir hnrh\n\
\101456 rtqfwbl\n\
\$ cd fpbrwnz\n\
\$ ls\n\
\232867 btmz\n\
\150179 cmq.tgm\n\
\249603 jztmgg.dlb\n\
\dir mqhz\n\
\62465 wdcsgg.cjt\n\
\$ cd mqhz\n\
\$ ls\n\
\232359 rdzsz.lhj\n\
\25201 vpjbmjd.zvt\n\
\277414 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd hnrh\n\
\$ ls\n\
\dir dmghrm\n\
\290254 lcpgtrc.dqm\n\
\dir tbbp\n\
\104510 vqqcvgts.vrc\n\
\$ cd dmghrm\n\
\$ ls\n\
\16799 vqqcvgts.vrc\n\
\$ cd ..\n\
\$ cd tbbp\n\
\$ ls\n\
\14688 jrvt\n\
\45492 wdcsgg.cjt\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd nwqgchw\n\
\$ ls\n\
\21374 lhfsc.lrh\n\
\121726 wdcsgg.cjt"

lns = lines input

isCommand ('$':_) = True
isCommand _       = False

split ch text =
    let i = findIndex (==ch) text
    in  case i of Nothing    -> [text]
                  Just index -> result
                        where (part, rest) = splitAt index text
                              result = part : split ch (tail rest)
unite segments = '/' : (intercalate "/" segments)

segments path = filter (/="") $ split '/' path

parent path =
    let s = segments path
        len = length s
    in  unite $ take (len - 1) s

child name path =
    let s = segments path
    in  unite $ s ++ [name]

files output =
    let d ("dir":xs) = []
        d [size, name] = [(name, read size :: Int)]
    in  intercalate [] $ map (d . words) output

run path [] = []
run path (o:ops) =
    let w = words o
        (output:xs) = groupBy (\a b -> isCommand a == isCommand b) ops
        rest = run path $ drop (length output) ops
        f = files output
        result = map (\(name, size) -> (child name path, size)) f
    in  case w of ["$", "cd", ".."] -> run (parent path) ops
                  ["$", "cd", name] -> run (child name path) ops
                  ["$", "ls"]       -> result ++ rest
                                    
allFiles = run "/" lns

exceptLast [] = []
exceptLast [a] = []
exceptLast (x:xs) = x : exceptLast xs

allFolders = sort $ nub $ intercalate [] $ map (map unite . exceptLast . inits . segments . fst) allFiles

calc folder =
    let fs = segments folder
        matched = filter (\(name, _) -> fs `isPrefixOf` (segments name)) allFiles
    in  sum $ map snd matched

answer1 = sum $ map calc $ filter (\x -> calc x <= 100000) allFolders

---

totalSize = 70000000
targetFreeSize = 30000000
rootSize = calc "/"

needToFree =
    let currentFree = totalSize - rootSize
    in  targetFreeSize - currentFree

sizedFolders = map (\x -> (calc x, x)) allFolders
answer2 = minimum $ filter (\(size, _) -> size >= needToFree) $ sizedFolders