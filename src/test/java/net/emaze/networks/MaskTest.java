package net.emaze.networks;

import net.emaze.networks.FixedSizeNatural;
import net.emaze.networks.Mask;
import net.emaze.networks.IpPolicy;
import org.junit.Assert;
import org.junit.Test;

public class MaskTest {

    public static final IpPolicy V4 = new IpPolicy.V4();
    public static final IpPolicy V6 = new IpPolicy.V6();

    @Test(expected = IllegalArgumentException.class)
    public void cannotCreateMaskBiggerThanPolicyAllows() {
        new Mask(33, V4);
    }

    @Test(expected = IllegalArgumentException.class)
    public void cannotCreateMaskWithNegativeSize() {
        new Mask(-1, V4);
    }

    @Test
    public void canExtractFirstOctet() {
        Assert.assertEquals(FixedSizeNatural.of(0xFF000000), Mask.netV4(8).bits());
    }

    @Test
    public void netCreatesAMaskWithPopulationOfLeadingOnes() {
        Assert.assertEquals(FixedSizeNatural.of(0xffffff00), Mask.netV4(24).bits());
    }

    @Test
    public void netOf32CreatesMaskOfOnlyOnes() {
        Assert.assertEquals(FixedSizeNatural.of(0xffffffff), Mask.netV4(32).bits());
    }

    @Test
    public void netOf0CreatesMaskOfOnlyZeros() {
        Assert.assertEquals(FixedSizeNatural.of(0x00000000), Mask.netV4(0).bits());
    }

    @Test
    public void canCreateIpv6Netmask() {
        Assert.assertEquals(V6.getWidestMask(), Mask.netV6(0));
    }

    @Test
    public void hostCreatesAMaskWithPopulationOfTrailingOnes() {
        Assert.assertEquals(FixedSizeNatural.of(0x00ffffff), Mask.hostV4(24).hostBits());
    }

    @Test
    public void hostOf32CreatesMaskOfOnlyOnes() {
        Assert.assertEquals(FixedSizeNatural.of(0xffffffff), Mask.hostV4(32).hostBits());
    }

    @Test
    public void hostOf0CreatesMaskOfOnlyZeros() {
        Assert.assertEquals(FixedSizeNatural.of(0x00000000), Mask.hostV4(0).hostBits());
    }

    @Test
    public void canCreateIpv6Hostmask() {
        Assert.assertEquals(V6.getNarrowestMask(), Mask.hostV6(0));
    }

    @Test
    public void hostMaskIsComplementaryToNetMask() {
        final Mask net = Mask.netV4(16);
        final Mask host = Mask.hostV4(16);
        Assert.assertEquals(net.bits().and(host.hostBits()), FixedSizeNatural.zero(32));
    }

    @Test
    public void narrowerYieldsANarrowerNetmask() {
        final Mask narrower = Mask.netV4(16).narrowHosts();
        Assert.assertEquals(Mask.netV4(17), narrower);
    }

    @Test
    public void narrowerOfWidestIsANetOf1() {
        Assert.assertEquals(Mask.netV4(1), V4.getWidestMask().narrowHosts());
    }

    @Test
    public void narrowingNarrowestYieldsItself() {
        Assert.assertEquals(V4.getNarrowestMask(), V4.getNarrowestMask().narrowHosts());
    }

    @Test
    public void isNarrowestYieldsTrueWhenNetmaskHas32bits() {
        Assert.assertEquals(V4.getNarrowestMask(), Mask.netV4(32));
    }

    @Test
    public void widerYieldsAWiderNetmask() {
        final Mask wider = Mask.netV4(16).widenHosts();
        Assert.assertEquals(Mask.netV4(15), wider);
    }

    @Test
    public void widerOfNarrowestIsANetOf31() {
        final Mask wider = V4.getNarrowestMask().widenHosts();
        Assert.assertEquals(Mask.netV4(31), wider);
    }

    @Test
    public void wideningWidestMaskYieldsItself() {
        Assert.assertEquals(V4.getWidestMask(), V4.getWidestMask().widenHosts());
    }

    @Test
    public void zeroBitsNetMaskIsWidest() {
        Assert.assertEquals(V4.getWidestMask(), Mask.netV4(0));
    }

    @Test
    public void zeroBitsHostMaskIsNarrowestNetmask() {
        Assert.assertEquals(V4.getNarrowestMask(), Mask.hostV4(0));
    }

    @Test
    public void thirtyTwoBitsHostMaskIsWidestNetmask() {
        Assert.assertEquals(V4.getWidestMask(), Mask.hostV4(32));
    }

    @Test
    public void thirtyTwoBitsNetMaskIsNarrowest() {
        Assert.assertEquals(V4.getNarrowestMask(), Mask.netV4(32));
    }

    @Test
    public void netmaskFromSameLongAreEquals() {
        Assert.assertEquals(Mask.netV4(16), Mask.netV4(16));
    }

    @Test
    public void sameNetmaskInDifferentFormatsAreEquals() {
        Assert.assertEquals(Mask.netV4(16), Mask.netV4("255.255.0.0"));
    }

    @Test
    public void netmaskOfDifferentBitsAreDifferent() {
        Assert.assertFalse(Mask.netV4(16).equals(Mask.netV4(17)));
    }

    @Test
    public void netmaskIsDifferentFromNull() {
        Assert.assertFalse(Mask.netV4(16).equals(null));
    }

    @Test
    public void netmaskIsDifferentFromOtherObjects() {
        Assert.assertFalse(Mask.netV4(16).equals(new Object()));
    }

    @Test
    public void toStringOfWidestIs() {
        Assert.assertEquals("0.0.0.0", V4.getWidestMask().toString());
    }

    @Test
    public void toStringOfNarrowestIs() {
        Assert.assertEquals("255.255.255.255", V4.getNarrowestMask().toString());
    }

    @Test
    public void parseValidNetmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Mask.netV4("255.0.0.0").population());
    }

    @Test
    public void parseValidHostmaskYieldsMaskWithExpectedPopulation() {
        Assert.assertEquals(8, Mask.hostV4("0.0.0.255").hostPopulation());
    }

    @Test(expected = IllegalArgumentException.class)
    public void malformedMaskThrows() {
        Assert.assertEquals(8, Mask.netV4("255.0.0.255").population());
    }

    @Test
    public void hostsOfNarrowestNetmaskIsOne() {
        Assert.assertEquals(FixedSizeNatural.of(1), V4.getNarrowestMask().hosts());
    }

    @Test
    public void hostsOfWidestNetmaskIsLots() {
        Assert.assertEquals(FixedSizeNatural.of(1L << 32), V4.getWidestMask().hosts());
    }

}
