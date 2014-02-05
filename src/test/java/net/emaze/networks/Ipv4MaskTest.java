package net.emaze.networks;

import java.math.BigInteger;
import junit.framework.Assert;
import org.junit.Test;

public class Ipv4MaskTest {

    @Test
    public void netCreatesAMaskWithPopulationOfLeadingOnes() {
        final FixedSizeNatural bits = Ipv4Mask.net(24).bits();
        Assert.assertEquals(FixedSizeNatural.of(0xffffff00), bits);
    }

    @Test
    public void netOf32CreatesMaskOfOnlyOnes() {
        final FixedSizeNatural bits = Ipv4Mask.net(32).bits();
        Assert.assertEquals(FixedSizeNatural.of(0xffffffff), bits);
    }

    @Test
    public void netOf0CreatesMaskOfOnlyZeros() {
        final FixedSizeNatural bits = Ipv4Mask.net(0).bits();
        Assert.assertEquals(FixedSizeNatural.of(0x00000000), bits);
    }

    @Test
    public void hostCreatesAMaskWithPopulationOfTrailingOnes() {
        final FixedSizeNatural bits = Ipv4Mask.host(24).hostBits();
        Assert.assertEquals(FixedSizeNatural.of(0x00ffffff), bits);
    }

    @Test
    public void hostOf32CreatesMaskOfOnlyOnes() {
        final FixedSizeNatural bits = Ipv4Mask.host(32).hostBits();
        Assert.assertEquals(FixedSizeNatural.of(0xffffffff), bits);
    }

    @Test
    public void hostOf0CreatesMaskOfOnlyZeros() {
        final FixedSizeNatural bits = Ipv4Mask.host(0).hostBits();
        Assert.assertEquals(FixedSizeNatural.of(0x00000000), bits);
    }

    @Test
    public void narrowerYieldsANarrowerNetmask() {
        final Ipv4Mask narrower = Ipv4Mask.net(16).narrowHosts();
        Assert.assertEquals(Ipv4Mask.net(17), narrower);
    }

    @Test
    public void narrowerOfWidestIsANetOf1() {
        final Ipv4Mask narrower = Ipv4Mask.getWidestMask().narrowHosts();
        Assert.assertEquals(Ipv4Mask.net(1), narrower);
    }

    @Test
    public void narrowingNarrowestYieldsItself() {
        Assert.assertEquals(Ipv4Mask.getNarrowestMask(), Ipv4Mask.getNarrowestMask().narrowHosts());
    }

    @Test
    public void isNarrowestYieldsTrueWhenNetmaskHas32bits() {
        Assert.assertEquals(Ipv4Mask.getNarrowestMask(), Ipv4Mask.net(32));
    }

    @Test
    public void widerYieldsAWiderNetmask() {
        final Ipv4Mask wider = Ipv4Mask.net(16).widenHosts();
        Assert.assertEquals(Ipv4Mask.net(15), wider);
    }

    @Test
    public void widerOfNarrowestIsANetOf31() {
        final Ipv4Mask wider = Ipv4Mask.getNarrowestMask().widenHosts();
        Assert.assertEquals(Ipv4Mask.net(31), wider);
    }

    @Test
    public void wideningWidestMaskYieldsItself() {
        Assert.assertEquals(Ipv4Mask.getWidestMask(), Ipv4Mask.getWidestMask().widenHosts());
    }

    @Test
    public void zeroBitsNetMaskIsWidest() {
        Assert.assertEquals(Ipv4Mask.getWidestMask(), Ipv4Mask.net(0));
    }

    @Test
    public void zeroBitsHostMaskIsNarrowestNetmask() {
        Assert.assertEquals(Ipv4Mask.getNarrowestMask(), Ipv4Mask.host(0));
    }

    @Test
    public void thirtyTwoBitsHostMaskIsWidestNetmask() {
        Assert.assertEquals(Ipv4Mask.getWidestMask(), Ipv4Mask.host(32));
    }

    @Test
    public void thirtyTwoBitsNetMaskIsNarrowest() {
        Assert.assertEquals(Ipv4Mask.getNarrowestMask(), Ipv4Mask.net(32));
    }

    @Test
    public void netmaskFromSameLongAreEquals() {
        Assert.assertEquals(Ipv4Mask.net(16), Ipv4Mask.net(16));
    }

    @Test
    public void sameNetmaskInDifferentFormatsAreEquals() {
        Assert.assertEquals(Ipv4Mask.net(16), Ipv4Mask.net("255.255.0.0"));
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
        Assert.assertEquals("0.0.0.0", Ipv4Mask.getWidestMask().toString());
    }

    @Test
    public void toStringOfNarrowestIs() {
        Assert.assertEquals("255.255.255.255", Ipv4Mask.getNarrowestMask().toString());
    }

    @Test
    public void parseValidNetmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Ipv4Mask.net("255.0.0.0").population());
    }

    @Test
    public void parseValidHostmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Ipv4Mask.host("0.0.0.255").hostPopulation());
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedMaskThrows() {
        Assert.assertEquals(8, Ipv4Mask.net("255.0.0.255").population());
    }

    @Test
    public void hostsOfNarrowestNetmaskIsOne() {
        Assert.assertEquals(BigInteger.valueOf(1L), Ipv4Mask.getNarrowestMask().hosts());
    }

    @Test
    public void hostsOfWidestNetmaskIsLots() {
        Assert.assertEquals(BigInteger.ONE.shiftLeft(32), Ipv4Mask.getWidestMask().hosts());
    }
}
