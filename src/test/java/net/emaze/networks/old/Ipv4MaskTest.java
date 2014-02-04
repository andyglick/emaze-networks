package net.emaze.networks.old;

import net.emaze.networks.old.Ipv4Mask;
import junit.framework.Assert;
import org.junit.Test;

public class Ipv4MaskTest {

    @Test
    public void canExtractFirstOctet() {
        Assert.assertEquals(255, Ipv4Mask.net(8).octets()[0]);
    }

    @Test
    public void netCreatesAMaskWithPopulationOfLeadingOnes() {
        int bits = Ipv4Mask.net(24).bits();
        Assert.assertEquals(0xffffff00, bits);
    }

    @Test
    public void netOf32CreatesMaskOfOnlyOnes() {
        int bits = Ipv4Mask.net(32).bits();
        Assert.assertEquals(0xffffffff, bits);
    }

    @Test
    public void netOf0CreatesMaskOfOnlyZeros() {
        int bits = Ipv4Mask.net(0).bits();
        Assert.assertEquals(0x00000000, bits);
    }

    @Test
    public void hostCreatesAMaskWithPopulationOfTrailingOnes() {
        int bits = Ipv4Mask.host(24).bits();
        Assert.assertEquals(0x00ffffff, bits);
    }

    @Test
    public void hostOf32CreatesMaskOfOnlyOnes() {
        int bits = Ipv4Mask.host(32).bits();
        Assert.assertEquals(0xffffffff, bits);
    }

    @Test
    public void hostOf0CreatesMaskOfOnlyZeros() {
        int bits = Ipv4Mask.host(0).bits();
        Assert.assertEquals(0x00000000, bits);
    }

    @Test
    public void narrowerYieldsANarrowerNetmask() {
        final Ipv4Mask narrower = Ipv4Mask.net(16).narrowHosts();
        Assert.assertEquals(Ipv4Mask.net(17), narrower);
    }

    @Test
    public void narrowerOfWidestIsANetOf1() {
        final Ipv4Mask narrower = Ipv4Mask.WIDEST.narrowHosts();
        Assert.assertEquals(Ipv4Mask.net(1), narrower);
    }

    @Test
    public void narrowingNarrowestYieldsItself() {
        Assert.assertEquals(Ipv4Mask.NARROWEST, Ipv4Mask.NARROWEST.narrowHosts());
    }

    @Test
    public void isNarrowestYieldsTrueWhenNetmaskHas32bits() {
        Assert.assertEquals(Ipv4Mask.NARROWEST, Ipv4Mask.net(32));
    }

    @Test
    public void widerYieldsAWiderNetmask() {
        final Ipv4Mask wider = Ipv4Mask.net(16).widenHosts();
        Assert.assertEquals(Ipv4Mask.net(15), wider);
    }

    @Test
    public void widerOfNarrowestIsANetOf31() {
        final Ipv4Mask wider = Ipv4Mask.NARROWEST.widenHosts();
        Assert.assertEquals(Ipv4Mask.net(31), wider);
    }

    @Test
    public void wideningWidestMaskYieldsItself() {
        Assert.assertEquals(Ipv4Mask.WIDEST, Ipv4Mask.WIDEST.widenHosts());
    }

    @Test
    public void zeroBitsNetMaskIsWidest() {
        Assert.assertEquals(Ipv4Mask.WIDEST, Ipv4Mask.net(0));
    }

    @Test
    public void zeroBitsHostMaskIsWidest() {
        Assert.assertEquals(Ipv4Mask.WIDEST, Ipv4Mask.host(0));
    }

    @Test
    public void thirtyTwoBitsHostMaskIsNarrowest() {
        Assert.assertEquals(Ipv4Mask.NARROWEST, Ipv4Mask.host(32));
    }

    @Test
    public void thirtyTwoBitsNetMaskIsNarrowest() {
        Assert.assertEquals(Ipv4Mask.NARROWEST, Ipv4Mask.net(32));
    }

    @Test
    public void netmaskFromSameLongAreEquals() {
        Assert.assertEquals(Ipv4Mask.net(16), Ipv4Mask.net(16));
    }

    @Test
    public void sameNetmaskInDifferentFormatsAreEquals() {
        Assert.assertEquals(Ipv4Mask.net(16), Ipv4Mask.parse("255.255.0.0"));
    }

    @Test
    public void netmaskOfDifferentBitsAreDifferent() {
        Assert.assertFalse(Ipv4Mask.net(16).equals(Ipv4Mask.net(17)));
    }

    @Test
    public void netmaskIsDifferentFromNull() {
        Assert.assertFalse(Ipv4Mask.net(16).equals(null));
    }

    @Test
    public void netmaskIsDifferentFromOtherObjects() {
        Assert.assertFalse(Ipv4Mask.net(16).equals(new Object()));
    }

    @Test
    public void toStringOfWidestIs() {
        Assert.assertEquals("0.0.0.0", Ipv4Mask.WIDEST.toString());
    }

    @Test
    public void toStringOfNarrowestIs() {
        Assert.assertEquals("255.255.255.255", Ipv4Mask.NARROWEST.toString());
    }

    @Test
    public void parseValidNetmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Ipv4Mask.parse("255.0.0.0").population());
    }

    @Test
    public void parseValidHostmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Ipv4Mask.parse("0.0.0.255").population());
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedMaskThrows() {
        Assert.assertEquals(8, Ipv4Mask.parse("255.0.0.255").population());
    }

    @Test
    public void widestIsHostmask() {
        Assert.assertEquals(true, Ipv4Mask.WIDEST.isHostmask());
    }

    @Test
    public void narrowestIsHostmask() {
        Assert.assertEquals(true, Ipv4Mask.NARROWEST.isHostmask());
    }

    @Test
    public void whenPopulationIsOnLowestBitsIsHostmask() {
        Assert.assertEquals(true, Ipv4Mask.parse("0.0.0.1").isHostmask());
    }

    @Test
    public void widestIsNetmask() {
        Assert.assertEquals(true, Ipv4Mask.WIDEST.isNetmask());
    }

    @Test
    public void narrowestIsNetmask() {
        Assert.assertEquals(true, Ipv4Mask.NARROWEST.isNetmask());
    }

    @Test
    public void whenPopulationOnHighestBitsIsNetmask() {
        Assert.assertEquals(true, Ipv4Mask.parse("128.0.0.0").isNetmask());
    }

    @Test
    public void hostsOfNarrowestNetmaskIsOne() {
        Assert.assertEquals(1L, Ipv4Mask.NARROWEST.hosts());
    }

    @Test
    public void hostsOfWidestNetmaskIsLots() {
        Assert.assertEquals(1L << 32, Ipv4Mask.WIDEST.hosts());
    }
}
