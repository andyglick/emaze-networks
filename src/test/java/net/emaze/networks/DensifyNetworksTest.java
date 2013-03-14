package net.emaze.networks;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.Assert;
import org.junit.Test;

public class DensifyNetworksTest {

    @Test
    public void contiguousCidrsOfSameNetmaskWillBeJoined() {
        final Network lhs = Network.fromCidrNotation("192.168.0.0/24");
        final Network rhs = Network.fromCidrNotation("192.168.1.0/24");
        final Set<Network> expected = Collections.singleton(Network.fromCidrNotation("192.168.0.0/23"));
        Assert.assertEquals(expected, new DensifyNetworks().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void noncontiguousCidrsWillNotBeJoined() {
        final Network lhs = Network.fromCidrNotation("192.168.0.0/24");
        final Network rhs = Network.fromCidrNotation("192.168.2.0/24");
        final Set<Network> expected = new HashSet<>(Arrays.asList(lhs, rhs));
        Assert.assertEquals(expected, new DensifyNetworks().perform(Arrays.asList(lhs, rhs)));
    }

    @Test
    public void canJoinNoncontiguousSetsOfContiguousCidrs() {
        final Collection<Network> toBeJoined = Arrays.asList(
                Network.fromCidrNotation("192.168.0.0/24"),
                Network.fromCidrNotation("192.168.1.0/24"),
                Network.fromCidrNotation("192.168.16.0/24"),
                Network.fromCidrNotation("192.168.17.0/24"));
        final Set<Network> expected = new HashSet<>(Arrays.asList(
                Network.fromCidrNotation("192.168.0.0/23"),
                Network.fromCidrNotation("192.168.16.0/23")));
        Assert.assertEquals(expected, new DensifyNetworks().perform(toBeJoined));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullCidrsThrows() {
        new DensifyNetworks().perform(null);
    }
}
