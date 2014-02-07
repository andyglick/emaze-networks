package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import net.emaze.networks.Ipv4Mask;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4MaskToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'netmask':'255.255.255.0'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv4Mask bwn = new BeanWithIpv4Mask();
        bwn.setNetmask(Ipv4Mask.net(24));
        final String got = mapper.writeValueAsString(bwn);
        Assert.assertEquals(expected, got);
    }

}
