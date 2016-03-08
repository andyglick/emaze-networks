package net.emaze.networks.ipv6;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6SubtractNetworksFromNetworkTest {

    @Test
    public void canSubtractContainedFromContainer() {
        final Ipv6Network minuend = Ipv6Network.fromCidrNotation("1234::/16");
        final Ipv6Network subtrahend = Ipv6Network.fromCidrNotation("1234::/17");
        final Ipv6Network expected = Ipv6Network.fromCidrNotation("1234:8000:/17");
        Assert.assertEquals(Collections.singleton(expected), new Ipv6SubtractNetworksFromNetwork().apply(minuend, Collections.singleton(subtrahend)));
    }

    @Test
    public void canSubtractCidrFromItself() {
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("1234::/16");
        Assert.assertEquals(Collections.emptySet(), new Ipv6SubtractNetworksFromNetwork().apply(cidr, Collections.singleton(cidr)));
    }

    @Test
    public void subtractingNonOverlappingRangesYieldsMinuend() {
        final Ipv6Network minuend = Ipv6Network.fromCidrNotation("1234::/16");
        final Ipv6Network nonOverlappingSubtrahend = Ipv6Network.fromCidrNotation("4321::/16");
        Assert.assertEquals(Collections.singleton(minuend), new Ipv6SubtractNetworksFromNetwork().apply(minuend, Collections.singleton(nonOverlappingSubtrahend)));
    }

    @Test
    public void subtractingContainerFromContainedYieldsEmptySet() {
        final Ipv6Network minuend = Ipv6Network.fromCidrNotation("1234::/17");
        final Ipv6Network subtrahend = Ipv6Network.fromCidrNotation("1234::/16");
        Assert.assertEquals(Collections.emptySet(), new Ipv6SubtractNetworksFromNetwork().apply(minuend, Collections.singleton(subtrahend)));
    }

    @Test
    public void SubtractingSmallCidrFromBigCidrYieldsLotsOfCidrs() {
        final Ipv6Network minuend = Ipv6Network.fromCidrNotation("1234::/16");
        final Ipv6Network subtrahend = Ipv6Network.fromCidrNotation("1234::/20");
        final Set<Ipv6Network> expected = new HashSet<>(Arrays.asList(
                Ipv6Network.fromCidrNotation("1234:8000:/17"),
                Ipv6Network.fromCidrNotation("1234:1000:/20"),
                Ipv6Network.fromCidrNotation("1234:2000:/19"),
                Ipv6Network.fromCidrNotation("1234:4000:/18")
        ));
        Assert.assertEquals(expected, new Ipv6SubtractNetworksFromNetwork().apply(minuend, Collections.singleton(subtrahend)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullMinuendThrows() {
        new Ipv6SubtractNetworksFromNetwork().apply(null, Collections.<Ipv6Network>emptySet());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullSubtrahendThrows() {
        new Ipv6SubtractNetworksFromNetwork().apply(Ipv6Network.fromCidrNotation("1234::/16"), null);
    }
}
