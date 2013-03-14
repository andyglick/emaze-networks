package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class MaskTest {

    @Test
    public void netCreatesAMaskWithPopulationOfLeadingOnes() {
        int bits = Mask.net(24).bits();
        Assert.assertEquals(0xffffff00, bits);
    }

    @Test
    public void netOf32CreatesMaskOfOnlyOnes() {
        int bits = Mask.net(32).bits();
        Assert.assertEquals(0xffffffff, bits);
    }

    @Test
    public void netOf0CreatesMaskOfOnlyZeros() {
        int bits = Mask.net(0).bits();
        Assert.assertEquals(0x00000000, bits);
    }

    @Test
    public void hostCreatesAMaskWithPopulationOfTrailingOnes() {
        int bits = Mask.host(24).bits();
        Assert.assertEquals(0x00ffffff, bits);
    }

    @Test
    public void hostOf32CreatesMaskOfOnlyOnes() {
        int bits = Mask.host(32).bits();
        Assert.assertEquals(0xffffffff, bits);
    }

    @Test
    public void hostOf0CreatesMaskOfOnlyZeros() {
        int bits = Mask.host(0).bits();
        Assert.assertEquals(0x00000000, bits);
    }

    @Test
    public void narrowerYieldsANarrowerNetmask() {
        final Mask narrower = Mask.net(16).narrowHosts();
        Assert.assertEquals(Mask.net(17), narrower);
    }

    @Test
    public void narrowerOfWidestIsANetOf1() {
        final Mask narrower = Mask.WIDEST.narrowHosts();
        Assert.assertEquals(Mask.net(1), narrower);
    }

    @Test
    public void narrowingNarrowestYieldsItself() {
        Assert.assertEquals(Mask.NARROWEST, Mask.NARROWEST.narrowHosts());
    }

    @Test
    public void isNarrowestYieldsTrueWhenNetmaskHas32bits() {
        Assert.assertEquals(Mask.NARROWEST, Mask.net(32));
    }

    @Test
    public void widerYieldsAWiderNetmask() {
        final Mask wider = Mask.net(16).widenHosts();
        Assert.assertEquals(Mask.net(15), wider);
    }

    @Test
    public void widerOfNarrowestIsANetOf31() {
        final Mask wider = Mask.NARROWEST.widenHosts();
        Assert.assertEquals(Mask.net(31), wider);
    }

    @Test
    public void wideningWidestMaskYieldsItself() {
        Assert.assertEquals(Mask.WIDEST, Mask.WIDEST.widenHosts());
    }

    @Test
    public void zeroBitsNetMaskIsWidest() {
        Assert.assertEquals(Mask.WIDEST, Mask.net(0));
    }

    @Test
    public void zeroBitsHostMaskIsWidest() {
        Assert.assertEquals(Mask.WIDEST, Mask.host(0));
    }

    @Test
    public void thirtyTwoBitsHostMaskIsNarrowest() {
        Assert.assertEquals(Mask.NARROWEST, Mask.host(32));
    }

    @Test
    public void thirtyTwoBitsNetMaskIsNarrowest() {
        Assert.assertEquals(Mask.NARROWEST, Mask.net(32));
    }

    @Test
    public void netmaskFromSameLongAreEquals() {
        Assert.assertEquals(Mask.net(16), Mask.net(16));
    }

    @Test
    public void sameNetmaskInDifferentFormatsAreEquals() {
        Assert.assertEquals(Mask.net(16), Mask.parse("255.255.0.0"));
    }

    @Test
    public void netmaskOfDifferentBitsAreDifferent() {
        Assert.assertFalse(Mask.net(16).equals(Mask.net(17)));
    }

    @Test
    public void netmaskIsDifferentFromNull() {
        Assert.assertFalse(Mask.net(16).equals(null));
    }

    @Test
    public void netmaskIsDifferentFromOtherObjects() {
        Assert.assertFalse(Mask.net(16).equals(new Object()));
    }

    @Test
    public void toStringOfWidestIs() {
        Assert.assertEquals("0.0.0.0", Mask.WIDEST.toString());
    }

    @Test
    public void toStringOfNarrowestIs() {
        Assert.assertEquals("255.255.255.255", Mask.NARROWEST.toString());
    }

    @Test
    public void parseValidNetmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Mask.parse("255.0.0.0").population());
    }

    @Test
    public void parseValidHostmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Mask.parse("0.0.0.255").population());
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedMaskThrows() {
        Assert.assertEquals(8, Mask.parse("255.0.0.255").population());
    }
}
