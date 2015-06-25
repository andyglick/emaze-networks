package net.emaze.networks.ipv6;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6DensifyNetworksTest {

    @Test
    public void contiguousCidrsOfSameNetmaskWillBeJoined() {
        final Ipv6Network lhs = Ipv6Network.fromCidrNotation("1234::/17");
        final Ipv6Network rhs = Ipv6Network.fromCidrNotation("1234:8000::/17");
        final Set<Ipv6Network> expected = Collections.singleton(Ipv6Network.fromCidrNotation("1234::/16"));
        Assert.assertEquals(expected, new Ipv6DensifyNetworks().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void noncontiguousCidrsWillNotBeJoined() {
        final Ipv6Network lhs = Ipv6Network.fromCidrNotation("1234::/17");
        final Ipv6Network rhs = Ipv6Network.fromCidrNotation("4321::/17");
        final HashSet<Ipv6Network> expected = new HashSet<>(Arrays.asList(lhs, rhs));
        Assert.assertEquals(expected, new Ipv6DensifyNetworks().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void canJoinNoncontiguousSetsOfContiguousCidrs() {
        final List<Ipv6Network> toBeJoined = Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::/17"),
                Ipv6Network.fromCidrNotation("1234:8000:/17"),
                Ipv6Network.fromCidrNotation("4321:8000:/18"),
                Ipv6Network.fromCidrNotation("4321:C000:/18"));
        final HashSet<Ipv6Network> expected = new HashSet<>(Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::/16"),
                Ipv6Network.fromCidrNotation("4321:8000:/17")));
        Assert.assertEquals(expected, new Ipv6DensifyNetworks().perform(toBeJoined));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullCidrsThrows() {
        new Ipv6DensifyNetworks().perform(null);
    }
}
